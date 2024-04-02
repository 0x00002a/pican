use crate::{
    context::{IrContext, PicanContext},
    diagnostics::FatalErrorEmitted,
};

use crate::pir;

use super::ast;

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

    use crate::{
        alloc::{Bump, BumpVec},
        context::PicanContext,
        copy_arrayvec::CopyArrayVec,
        diagnostics::{DiagnosticBuilder, FatalErrorEmitted},
        ir::{Ident, IrNode, SwizzleDim, SwizzleDims},
    };

    use crate::frontend::ast::{
        Constant, FunctionDecl, InputBind, Op, Operand, OperandKind, Operands, OutputBind,
        RegisterBindTarget, Statement, SwizzleExpr, UniformTy,
    };
    use crate::pir::bindings::{self as pib, SwizzleValue};
    use crate::pir::ir as pir;

    pub struct PirLower<'a, 'c, S: AsRef<str>> {
        alloc: &'a Bump,
        ctx: &'c PicanContext<S>,
        bindings: pib::Bindings<'a>,
        entry_points: BumpVec<'a, IrNode<pir::EntryPoint<'a>>>,
        outputs: BumpVec<'a, IrNode<&'a pir::OutputBinding<'a>>>,
        inputs: BumpVec<'a, IrNode<&'a pir::InputBinding<'a>>>,
        no_dvle: bool,
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
                no_dvle: false,
            }
        }

        fn check_non_aliased(&self, var: IrNode<Ident>) -> Result<(), FatalErrorEmitted> {
            if let Some(id) = self.bindings.previous_definition(var) {
                return self.ctx.diag.fatal(
                    DiagnosticBuilder::error()
                        .at(&var)
                        .primary("identifier conflicts with existing one".to_string())
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
                no_dvle: self.no_dvle,
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
                        .map(|InputBind { ident, register }| {
                            self.check_non_aliased(ident)?;
                            Ok(self.alloc.alloc(pir::InputBinding {
                                name: ident.lower(self)?,
                                index: self.inputs.len(),
                                register,
                            }))
                        })
                        .transpose()?
                        .map(|b| -> &_ { b });
                    self.inputs.push(b);
                    self.bindings.define(b.get().name, b);
                    Ok(())
                }
                Statement::Directive(d) => match d.get() {
                    super::ast::Directive::NoDvle => {
                        self.no_dvle = true;
                        Ok(())
                    }
                    super::ast::Directive::Entry(_) => {
                        todo!("handle entrypoint for shader")
                    }
                    super::ast::Directive::Gsh => todo!("handle geometry shaders"),
                },
            }
        }
        fn lower<L: Lower>(&self, t: L) -> Result<L::Pir<'a>, FatalErrorEmitted> {
            t.lower(self)
        }
    }

    trait Lower {
        type Pir<'a>;
        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted>;
    }
    impl<'b> Lower for OutputBind<'b> {
        type Pir<'a> = pir::OutputBinding<'a>;

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
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

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
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
        type Pir<'a> = crate::pir::ty::UniformTy;

        fn lower<'a, S: AsRef<str>>(
            self,
            _ctx: &PirLower<'a, '_, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            use crate::pir::ty as pit;
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

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            Ok(self.copy_to(ctx.alloc))
        }
    }
    impl<T: Lower> Lower for IrNode<T> {
        type Pir<'a> = IrNode<T::Pir<'a>>;

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            self.map(|i| i.lower(ctx)).transpose()
        }
    }
    impl Lower for OperandKind<'_> {
        type Pir<'a> = pir::OperandKind<'a>;

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
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

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
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

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
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

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
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

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
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
                    RegisterBindTarget::Register(r) => Ok(match r.kind {
                        crate::register::RegisterKind::Input |
                        crate::register::RegisterKind::Output if !ctx.ctx.opts.picasso_compat_bug_for_bug => panic!("found alias for input or output, this should've been caught in an earlier pass"),
                        _ => {
                            pib::BindingValue::Register(*r)
                        }
                    } ),
                    RegisterBindTarget::Var(v) => Ok(pib::BindingValue::Alias(v.lower(ctx)?)),
                }
            }
        }
    }
    impl Lower for FunctionDecl<'_> {
        type Pir<'a> = pir::EntryPoint<'a>;

        fn lower<'a, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, '_, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            let name = ctx.lower(self.name)?;
            let mut failed = None;
            let ops = self
                .block
                .map(|b| {
                    b.statements.map(|stmts| {
                        let mut ops = BumpVec::new_in(ctx.alloc);
                        for stmt in stmts.iter().filter(|st| !st.get().is_comment()) {
                            let lowered = stmt
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
                                .transpose();
                            match lowered {
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
