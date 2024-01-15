use pican_core::{
    alloc::Bump,
    context::{IrContext, PicanContext},
    diagnostics::FatalErrorEmitted,
    ir::IrNode,
    PError, PResult,
};

use pican_pir as pir;

use crate::ast;

pub trait FrontendToPirCtx {
    fn lower<'a, S: AsRef<str>>(
        &'a self,
        ctx: &PicanContext<S>,
        ir: &[ast::Stmt],
    ) -> (pir::ir::Module<'a>, Option<FatalErrorEmitted>);
}
impl FrontendToPirCtx for IrContext {
    fn lower<'a, S: AsRef<str>>(
        &'a self,
        ctx: &PicanContext<S>,
        ir: &[ast::Stmt],
    ) -> (pir::ir::Module<'a>, Option<FatalErrorEmitted>) {
        let mut l = lowering::PirLower::new(self.arena(), ctx);
        let mut failed = None;
        for stmt in ir {
            if let Err(e) = l.lower_toplevel_stmt(*stmt.get()) {
                failed.replace(e);
            }
        }
        (l.build_module(), failed)
    }
}

mod lowering {
    use arrayvec::ArrayVec;
    use pican_core::{
        alloc::{Bump, BumpVec},
        context::PicanContext,
        copy_arrayvec::CopyArrayVec,
        diagnostics::{DiagnosticBuilder, FatalErrorEmitted},
        ir::{Float, Ident, IrNode, SwizzleDim, SwizzleDims},
        properties::OutputProperty,
        register::Register,
        PResult,
    };

    use crate::ast::{
        Constant, ConstantDecl, FunctionDecl, Op, Operand, OperandKind, Operands, OutputBind,
        RegisterBindTarget, Statement, Stmt, SwizzleExpr, UniformDecl, UniformTy,
    };
    use pican_pir::bindings::{self as pib, SwizzleValue};
    use pican_pir::ir as pir;

    pub struct PirLower<'a, 'c, S: AsRef<str>> {
        alloc: &'a Bump,
        ctx: &'c PicanContext<S>,
        bindings: pib::Bindings<'a>,
        entry_points: BumpVec<'a, IrNode<pir::EntryPoint<'a>>>,
        outputs: BumpVec<'a, IrNode<&'a pir::OutputBinding<'a>>>,
        inputs: BumpVec<'a, IrNode<&'a pir::InputBinding<'a>>>,
    }
    impl<'a, 'c, S: AsRef<str>> PirLower<'a, 'c, S> {
        pub fn new(alloc: &'a Bump, ctx: &'c PicanContext<S>) -> Self {
            Self {
                alloc,
                ctx,
                bindings: Default::default(),
                entry_points: BumpVec::new_in(alloc),
                outputs: BumpVec::new_in(alloc),
                inputs: BumpVec::new_in(alloc),
            }
        }

        fn check_non_aliased(&self, var: IrNode<Ident>) -> Result<(), FatalErrorEmitted> {
            if let Some(id) = self.bindings.previous_definition(var) {
                return self.ctx.diag.fatal(
                    DiagnosticBuilder::error()
                        .at(&var)
                        .primary(format!("identifier conflicts with existing one"))
                        .note(&id, "identifier previously bound here")
                        .build(),
                );
            }
            Ok(())
        }
        pub fn build_module(self) -> pir::Module<'a> {
            pir::Module {
                entry_points: self.entry_points.into_bump_slice(),
                bindings: self.bindings,
                outputs: self.outputs.into_bump_slice(),
                inputs: self.inputs.into_bump_slice(),
            }
        }
        fn unif_len_check<T>(&self, node: IrNode<&[T]>) -> Result<(), FatalErrorEmitted> {
            if node.get().len() != 4 {
                self.ctx.diag.fatal(
                    DiagnosticBuilder::error()
                        .at(&node)
                        .primary(format!(
                            "exactly 4 values are required for a constant, got {}",
                            node.get().len()
                        ))
                        .build(),
                )
            } else {
                Ok(())
            }
        }
        pub fn lower_toplevel_stmt(&mut self, stmt: Statement) -> Result<(), FatalErrorEmitted> {
            match stmt {
                Statement::EntryPoint(ep) => {
                    let pt = self.lower(ep)?;
                    self.entry_points.push(pt);
                    Ok(())
                }
                Statement::Op(o) => self.ctx.diag.fatal(
                    DiagnosticBuilder::error()
                        .at(&o)
                        .primary("cannot have operations outside of proc's")
                        .build(),
                ),
                Statement::RegisterBind(b) => {
                    // check its not aliases _before_ we lower it as we don't want to put
                    // it in the arena if its not used
                    self.check_non_aliased(b.get().name)?;
                    let name = b.get().name.lower(self)?;
                    self.bindings.define(name, b.get().reg.lower(self)?);
                    Ok(())
                }
                Statement::Uniform(u) => {
                    let ty = u.get().ty.lower(self)?;
                    for unif in u.get().uniforms.get().iter() {
                        self.check_non_aliased(unif.get().name)?;

                        // fixme: if this isn't the first one and fails then we catch higher up we "leak" arena mem
                        let name = unif.get().name.lower(self)?;
                        let dimension = unif.get().dimensions.map(|d| d.map(|i| i as usize));
                        self.bindings
                            .define(name, unif.map(|_| pir::Uniform { ty, dimension }));
                    }
                    Ok(())
                }
                Statement::Constant(c) => {
                    self.check_non_aliased(c.get().name)?;
                    let value = c.get().value.lower(self)?;
                    let name = c.get().name.lower(self)?;
                    self.bindings.define(name, value);
                    Ok(())
                }
                Statement::Comment(_) => Ok(()),
                Statement::OutputBind(o) => {
                    if let Some(ident) = o.get().alias {
                        self.check_non_aliased(ident)?;
                    }
                    let o = o.lower(self)?;
                    let o = o.map(|o| -> &_ { self.alloc.alloc(o) });
                    if let Some(ident) = o.get().alias {
                        self.bindings.define(ident, o);
                    }
                    self.outputs.push(o);
                    Ok(())
                }
                Statement::InputBind(i) => {
                    let b = i
                        .map(|i| {
                            self.check_non_aliased(i.0)?;
                            Ok(self.alloc.alloc(pir::InputBinding {
                                name: i.0.lower(self)?,
                                index: self.inputs.len(),
                            }))
                        })
                        .transpose()?
                        .map(|b| -> &_ { b });
                    self.inputs.push(b);
                    self.bindings.define(b.get().name, b);
                    Ok(())
                }
            }
        }
        fn lower<L: Lower>(&self, t: L) -> Result<L::Pir<'a>, FatalErrorEmitted> {
            t.lower(self)
        }
    }

    trait Lower {
        type Pir<'a>;
        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted>;
    }
    impl<'b> Lower for OutputBind<'b> {
        type Pir<'a> = pir::OutputBinding<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            Ok(pir::OutputBinding {
                register: self.register,
                alias: self.alias.map(|i| i.lower(ctx)).transpose()?,
                property: self.property,
            })
        }
    }
    impl<'b> Lower for Constant<'b> {
        type Pir<'a> = pir::ConstantUniform<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            match self {
                Constant::Integer(i) => {
                    ctx.unif_len_check(i)?;
                    Ok(pir::ConstantUniform::Integer(
                        i.map(|f| std::array::from_fn::<_, 4, _>(|i| f[i])),
                    ))
                }
                Constant::Float(f) => {
                    ctx.unif_len_check(f)?;
                    Ok(pir::ConstantUniform::Float(
                        f.map(|f| std::array::from_fn::<_, 4, _>(|i| f[i])),
                    ))
                }
                Constant::FloatArray { elements, hint } => {
                    if let Some(h) = hint {
                        if *h.get() as usize != elements.get().len() {
                            return ctx.ctx.diag.fatal(
                                DiagnosticBuilder::error()
                                    .at(&elements)
                                    .primary("hint size does not match actual size")
                                    .note(&h, "hint defined here")
                                    .build(),
                            );
                        }
                    }
                    let els = elements
                        .map(|elements| {
                            let mut els = BumpVec::with_capacity_in(elements.len(), ctx.alloc);
                            for el in elements.iter() {
                                ctx.unif_len_check(*el)?;
                                els.push(el.map(|el| std::array::from_fn(|i| el[i])));
                            }
                            Ok(els.into_bump_slice())
                        })
                        .transpose()?;
                    Ok(pir::ConstantUniform::FloatArray(els))
                }
            }
        }
    }

    impl Lower for UniformTy {
        type Pir<'a> = pican_pir::ty::UniformTy;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            _ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            use pican_pir::ty as pit;
            let t = match self {
                UniformTy::Bool => pit::UniformTy::Bool,
                UniformTy::Integer => pit::UniformTy::Integer,
                UniformTy::Float => pit::UniformTy::Float,
            };
            Ok(t)
        }
    }

    impl<'b> Lower for Ident<'b> {
        type Pir<'a> = Ident<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            Ok(self.copy_to(ctx.alloc))
        }
    }
    impl<T: Lower> Lower for IrNode<T> {
        type Pir<'a> = IrNode<T::Pir<'a>>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            self.map(|i| i.lower(ctx)).transpose()
        }
    }
    impl Lower for OperandKind<'_> {
        type Pir<'a> = pir::OperandKind<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            let r = match self {
                OperandKind::Var(v) => pir::OperandKind::Var(ctx.lower(v)?),
                OperandKind::Register(r) => pir::OperandKind::Register(r),
            };
            Ok(r)
        }
    }

    impl Lower for Operands<'_> {
        type Pir<'a> = CopyArrayVec<IrNode<pir::Operand<'a>>, 4>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            if self.0.len() > 4 {
                return ctx.ctx.diag.fatal(
                    DiagnosticBuilder::error()
                        .at(&self.0[4])
                        .primary("opcodes can have a maximum of 4 arguments")
                        .build(),
                );
            }
            let mut ops = CopyArrayVec::new();
            for op in self.0 {
                ops.push(ctx.lower(op.copied())?);
            }
            Ok(ops)
        }
    }
    impl Lower for Op<'_> {
        type Pir<'a> = pir::Op<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            let opcode = self.opcode;
            let operands = ctx.lower(self.operands)?;
            Ok(pir::Op { opcode, operands })
        }
    }
    impl<'b> Lower for SwizzleDims<'b> {
        type Pir<'a> = SwizzleDims<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            Ok(SwizzleDims(self.0.map(|s| -> &'a [SwizzleDim] {
                ctx.alloc.alloc_slice_copy(s)
            })))
        }
    }
    impl<'b> Lower for Operand<'b> {
        type Pir<'a> = pir::Operand<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            Ok(pir::Operand {
                kind: self.kind.lower(ctx)?,
                relative_addr: self.relative_address,
                swizzle: self.swizzle.map(|s| s.lower(ctx)).transpose()?,
            })
        }
    }

    impl<'b> Lower for SwizzleExpr<'b, RegisterBindTarget<'b>> {
        type Pir<'a> = pib::BindingValue<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            if let Some(swiz) = self.swizzle {
                let target = self.target.map(|target| match target {
                    RegisterBindTarget::Register(r) => {
                        Ok(pib::BindingValue::SwizzleRegister(SwizzleValue {
                            swizzle: swiz.lower(ctx)?,
                            target: self.target.map(|_| r),
                        }))
                    }
                    RegisterBindTarget::Var(v) => Ok(pib::BindingValue::SwizzleVar(SwizzleValue {
                        swizzle: swiz.lower(ctx)?,
                        target: self.target.map(|_| v.lower(ctx)).transpose()?,
                    })),
                });
                target.into_inner()
            } else {
                match self.target.get() {
                    RegisterBindTarget::Register(r) => Ok(pib::BindingValue::Register(*r)),
                    RegisterBindTarget::Var(v) => Ok(pib::BindingValue::Alias(v.lower(ctx)?)),
                }
            }
        }
    }
    impl Lower for FunctionDecl<'_> {
        type Pir<'a> = pir::EntryPoint<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            let name = ctx.lower(self.name)?;
            let mut failed = None;
            let ops = self
                .block
                .map(|b| {
                    b.statements.map(|stmts| {
                        let mut ops = BumpVec::new_in(ctx.alloc);
                        for stmt in stmts {
                            match stmt
                                .map(|st| {
                                    let Statement::Op(op) = st else {
                                        return ctx.ctx.diag.fatal(
                                    DiagnosticBuilder::error()
                                        .at(stmt)
                                        .primary("proc's are only allowed to constain operations")
                                        .build(),
                                );
                                    };
                                    ctx.lower(op)
                                })
                                .transpose()
                            {
                                Ok(op) => {
                                    ops.push(op.concat());
                                }
                                Err(e) => {
                                    failed.replace(e);
                                }
                            }
                        }
                        ops
                    })
                })
                .concat();
            if let Some(f) = failed {
                return Err(f);
            }
            let ops = ops.map(|v| v.into_bump_slice());
            Ok(pir::EntryPoint { name, ops })
        }
    }
}
