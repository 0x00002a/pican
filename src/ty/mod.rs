use crate::context::PicanContext;
use crate::diagnostics::FatalErrorEmitted;
use crate::frontend::ast::Ident;
use crate::ir::IrNode;
use crate::pir::ir::{Module, Operand};
use crate::register::RegisterKind;
use context::TyContext;
use typesum::sumtype;

pub mod check;
pub mod context;
pub mod ops;

pub trait PicanTyCheck {
    fn types_for_module<'a, 'b, 'c: 'b>(&'c self, m: &'b Module<'a>) -> TyContext<'a, 'b>;
}

impl<S: AsRef<str>> PicanTyCheck for PicanContext<S> {
    fn types_for_module<'a, 'b, 'c: 'b>(&'c self, m: &'b Module<'a>) -> TyContext<'a, 'b> {
        TyContext::new(m, &self.diag)
    }
}

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

impl<'a> ContextuallyTyped<'a> for IrNode<crate::pir::bindings::BindingValue<'a>> {
    fn ty_with_ctx<'b>(
        &self,
        ctx: &TyContext<'a, 'b>,
    ) -> Result<Type, crate::diagnostics::FatalErrorEmitted> {
        use crate::pir::bindings::BindingValue;
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
            crate::pir::ir::OperandKind::Var(v) => ctx.type_of(v),
            crate::pir::ir::OperandKind::Register(r) => ctx.type_of(&r.get().kind),
        }
    }
}

impl<'a, T: Typed> Typed for crate::pir::bindings::SwizzleValue<'a, T> {
    fn ty(&self) -> Type {
        self.target.get().ty()
    }
}
impl Typed for crate::pir::ir::Uniform {
    fn ty(&self) -> Type {
        let prim = match self.ty.get() {
            crate::pir::ty::UniformTy::Bool => PrimTy::Bool,
            crate::pir::ty::UniformTy::Integer => PrimTy::Integer,
            crate::pir::ty::UniformTy::Float => PrimTy::Float,
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

impl<'a> Typed for crate::pir::ir::ConstantUniform<'a> {
    fn ty(&self) -> Type {
        match self {
            crate::pir::ir::ConstantUniform::Integer(_) => {
                RegisterTy::new(PrimTy::Integer, RegisterKind::Input).into()
            }
            crate::pir::ir::ConstantUniform::Float(_) => {
                RegisterTy::new(PrimTy::Float, RegisterKind::Input).into()
            }
            crate::pir::ir::ConstantUniform::FloatArray(a) => UniformArrayTy {
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
