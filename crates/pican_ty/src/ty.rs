use pican_core::{
    diagnostics::FatalErrorEmitted,
    ir::{Ident, IrNode},
    register::RegisterKind,
};
use pican_pir::ir::Operand;
use typesum::sumtype;

use crate::context::TyContext;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[sumtype]
pub enum Type {
    UniformArray(UniformArrayTy),
    Register(RegisterTy),
    VecUniform(VecUniformTy),
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UniformArray(u) => u.fmt(f),
            Type::Register(r) => r.fmt(f),
            Type::VecUniform(u) => u.fmt(f),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PrimTy {
    Float,
    Integer,
    Bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct VecUniformTy {
    pub prim_ty: PrimTy,
}
impl std::fmt::Display for VecUniformTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Vec4<{:?}>", self.prim_ty))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct RegisterTy {
    pub prim_ty: PrimTy,
    pub kind: RegisterKind,
}
impl std::fmt::Display for RegisterTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("@{:?}<{:?}>", self.kind, self.prim_ty))
    }
}

impl RegisterTy {
    pub fn new(prim_ty: PrimTy, kind: RegisterKind) -> Self {
        Self { prim_ty, kind }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct UniformArrayTy {
    pub len: usize,
    pub element_ty: PrimTy,
}
impl std::fmt::Display for UniformArrayTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("[{:?}; {}]", self.element_ty, self.len))
    }
}

pub(crate) trait Typed {
    fn ty(&self) -> Type;
}

impl<'a> ContextuallyTyped<'a> for IrNode<pican_pir::bindings::BindingValue<'a>> {
    fn ty_with_ctx<'b>(
        &self,
        ctx: &TyContext<'a, 'b>,
    ) -> Result<Type, pican_core::diagnostics::FatalErrorEmitted> {
        use pican_pir::bindings::BindingValue;
        match self.get() {
            BindingValue::Register(r) => ctx.type_of(&r.kind),
            BindingValue::Uniform(u) => ctx.type_of(u),
            BindingValue::Constant(v) => ctx.type_of(v),
            BindingValue::OutputProperty(_) => {
                Ok(RegisterTy::new(PrimTy::Float, RegisterKind::Output).into())
            }
            BindingValue::Input(_) => {
                Ok(RegisterTy::new(PrimTy::Float, RegisterKind::Input).into())
            }
            BindingValue::SwizzleVar(s) => ctx.type_of(&s.target),
            BindingValue::SwizzleRegister(s) => ctx.type_of(&s.target.get().kind),
            BindingValue::Alias(i) => ctx.type_of(&self.map(|_| *i)),
        }
    }
}
impl<'a> ContextuallyTyped<'a> for IrNode<Ident<'a>> {
    fn ty_with_ctx<'b>(&self, ctx: &TyContext<'a, 'b>) -> Result<Type, FatalErrorEmitted> {
        ctx.lookup(*self)?.ty_with_ctx(ctx)
    }
}

impl<'a> ContextuallyTyped<'a> for Operand<'a> {
    fn ty_with_ctx<'b>(&self, ctx: &TyContext<'a, 'b>) -> Result<Type, FatalErrorEmitted> {
        match self.kind.get() {
            pican_pir::ir::OperandKind::Var(v) => ctx.type_of(v),
            pican_pir::ir::OperandKind::Register(r) => ctx.type_of(&r.get().kind),
        }
    }
}

impl<'a, T: Typed> Typed for pican_pir::bindings::SwizzleValue<'a, T> {
    fn ty(&self) -> Type {
        self.target.get().ty()
    }
}
impl Typed for pican_pir::ir::Uniform {
    fn ty(&self) -> Type {
        let prim = match self.ty.get() {
            pican_pir::ty::UniformTy::Bool => PrimTy::Bool,
            pican_pir::ty::UniformTy::Integer => PrimTy::Integer,
            pican_pir::ty::UniformTy::Float => PrimTy::Float,
        };
        if let Some(len) = self.dimension {
            UniformArrayTy {
                len: *len.get(),
                element_ty: prim,
            }
            .into()
        } else {
            VecUniformTy { prim_ty: prim }.into()
        }
    }
}

impl Typed for RegisterKind {
    fn ty(&self) -> Type {
        match self {
            k @ (RegisterKind::Scratch | RegisterKind::Output | RegisterKind::Input) => {
                RegisterTy::new(PrimTy::Float, *k).into()
            }
            RegisterKind::FloatingVecUniform => VecUniformTy {
                prim_ty: PrimTy::Float,
            }
            .into(),
            RegisterKind::IntegerVecUniform => VecUniformTy {
                prim_ty: PrimTy::Integer,
            }
            .into(),

            RegisterKind::BoolUniform => VecUniformTy {
                prim_ty: PrimTy::Bool,
            }
            .into(),
        }
    }
}

impl<'a> Typed for pican_pir::ir::ConstantUniform<'a> {
    fn ty(&self) -> Type {
        match self {
            pican_pir::ir::ConstantUniform::Integer(_) => {
                RegisterTy::new(PrimTy::Integer, RegisterKind::Input).into()
            }
            pican_pir::ir::ConstantUniform::Float(_) => {
                RegisterTy::new(PrimTy::Float, RegisterKind::Input).into()
            }
            pican_pir::ir::ConstantUniform::FloatArray(a) => UniformArrayTy {
                len: a.get().len(),
                element_ty: PrimTy::Float,
            }
            .into(),
        }
    }
}

pub trait ContextuallyTyped<'a> {
    fn ty_with_ctx<'b>(&self, ctx: &TyContext<'a, 'b>) -> Result<Type, FatalErrorEmitted>;
}

impl<'a, T> ContextuallyTyped<'a> for T
where
    T: Typed,
{
    fn ty_with_ctx<'b>(&self, _ctx: &TyContext<'a, 'b>) -> Result<Type, FatalErrorEmitted> {
        Ok(self.ty())
    }
}
