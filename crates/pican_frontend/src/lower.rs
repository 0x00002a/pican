use pican_core::{
    alloc::Bump,
    context::{IrContext, PicanContext},
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
    ) -> pir::ir::Module<'a>;
}
impl FrontendToPirCtx for IrContext {
    fn lower<'a, S: AsRef<str>>(
        &'a self,
        ctx: &PicanContext<S>,
        ir: &[ast::Stmt],
    ) -> pir::ir::Module<'a> {
        let mut l = lowering::PirLower::new(self.arena(), ctx);
        for stmt in ir {
            let _ = l.lower_toplevel_stmt(*stmt.get());
        }
        l.build_module()
    }
}

mod lowering {
    use arrayvec::ArrayVec;
    use pican_core::{
        alloc::{Bump, BumpVec},
        context::PicanContext,
        copy_arrayvec::CopyArrayVec,
        diagnostics::{DiagnosticBuilder, FatalErrorEmitted},
        ir::{Ident, IrNode},
        register::Register,
        PResult,
    };

    use crate::ast::{
        FunctionDecl, Op, Operand, Operands, Statement, Stmt, UniformDecl, UniformTy,
    };
    use pican_pir::bindings as pib;
    use pican_pir::ir as pir;

    pub struct PirLower<'a, 'c, S: AsRef<str>> {
        alloc: &'a Bump,
        ctx: &'c PicanContext<S>,
        bindings: pib::Bindings<'a>,
        entry_points: BumpVec<'a, IrNode<pir::EntryPoint<'a>>>,
    }
    impl<'a, 'c, S: AsRef<str>> PirLower<'a, 'c, S> {
        pub fn new(alloc: &'a Bump, ctx: &'c PicanContext<S>) -> Self {
            Self {
                alloc,
                ctx,
                bindings: Default::default(),
                entry_points: BumpVec::new_in(alloc),
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
                    self.bindings.define(name, b.get().reg);
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
                Statement::Constant(_) => todo!(),
                Statement::Comment(_) => Ok(()),
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
    impl Lower for Operand<'_> {
        type Pir<'a> = pir::Operand<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            let r = match self {
                Operand::Var(v) => pir::Operand::Var(ctx.lower(v)?),
                Operand::Register(r) => pir::Operand::Register(r),
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
    impl Lower for FunctionDecl<'_> {
        type Pir<'a> = pir::EntryPoint<'a>;

        fn lower<'a, 'c, S: AsRef<str>>(
            self,
            ctx: &PirLower<'a, 'c, S>,
        ) -> Result<Self::Pir<'a>, FatalErrorEmitted> {
            let name = ctx.lower(self.name)?;
            let ops = self
                .block
                .map(|b| {
                    b.statements.map(|stmts| {
                        let mut ops = BumpVec::new_in(ctx.alloc);
                        for stmt in stmts {
                            let Ok(op) = stmt
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
                            else {
                                continue;
                            };
                            ops.push(op.concat());
                        }
                        ops
                    })
                })
                .concat();
            let ops = ops.map(|v| v.into_bump_slice());
            Ok(pir::EntryPoint { name, ops })
        }
    }
}
