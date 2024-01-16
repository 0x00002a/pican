use pican_core::{ir::Ident, register::RegisterKind};
use pican_pir::{bindings::Bindings, ir::Operand};
use typesum::sumtype;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[sumtype]
pub enum Type {
    UniformArray(UniformArrayTy),
    Register(RegisterTy),
    VecUniform(VecUniformTy),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimTy {
    Float,
    Integer,
    Bool,
}
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VecUniformTy {
    pub prim_ty: PrimTy,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegisterTy {
    pub prim_ty: PrimTy,
}

impl RegisterTy {
    pub fn new(prim_ty: PrimTy) -> Self {
        Self { prim_ty }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct UniformArrayTy {
    pub len: usize,
    pub element_ty: PrimTy,
}

pub(crate) trait Typed {
    fn ty(&self) -> Type;
}

impl<'a> ContextuallyTyped<'a> for pican_pir::bindings::BindingValue<'a> {
    fn ty_with_ctx<'b>(&self, ctx: &TyContext<'a, 'b>) -> Type {
        use pican_pir::bindings::BindingValue;
        match self {
            BindingValue::Register(r) => r.kind.ty(),
            BindingValue::Uniform(u) => u.ty(),
            BindingValue::Constant(v) => v.ty(),
            BindingValue::OutputProperty(_) | BindingValue::Input(_) => {
                RegisterTy::new(PrimTy::Float).into()
            }
            BindingValue::SwizzleVar(s) => s.target.get().ty_with_ctx(ctx),
            BindingValue::SwizzleRegister(s) => s.target.get().kind.ty(),
            BindingValue::Alias(i) => i.ty_with_ctx(ctx),
        }
    }
}
impl<'a> ContextuallyTyped<'a> for Ident<'a> {
    fn ty_with_ctx<'b>(&self, ctx: &TyContext<'a, 'b>) -> Type {
        ctx.bindings
            .lookup(self)
            .unwrap_or_else(|| {
                panic!("couldn't find binding for {self:#?}, this should have been caught earlier");
            })
            .get()
            .ty_with_ctx(ctx)
    }
}

impl<'a> ContextuallyTyped<'a> for Operand<'a> {
    fn ty_with_ctx<'b>(&self, ctx: &TyContext<'a, 'b>) -> Type {
        match self.kind.get() {
            pican_pir::ir::OperandKind::Var(v) => v.get().ty_with_ctx(ctx),
            pican_pir::ir::OperandKind::Register(r) => r.get().kind.ty(),
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
            RegisterKind::Scratch | RegisterKind::Output | RegisterKind::Input => {
                RegisterTy::new(PrimTy::Float).into()
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
            pican_pir::ir::ConstantUniform::Integer(_) => RegisterTy::new(PrimTy::Integer).into(),
            pican_pir::ir::ConstantUniform::Float(_) => RegisterTy::new(PrimTy::Float).into(),
            pican_pir::ir::ConstantUniform::FloatArray(a) => UniformArrayTy {
                len: a.get().len(),
                element_ty: PrimTy::Float,
            }
            .into(),
        }
    }
}

pub trait ContextuallyTyped<'a> {
    fn ty_with_ctx<'b>(&self, ctx: &TyContext<'a, 'b>) -> Type;
}

pub struct TyContext<'a, 'b> {
    bindings: &'b Bindings<'a>,
}

impl<'a, 'b> TyContext<'a, 'b> {
    pub fn type_of(&self, expr: &impl ContextuallyTyped<'a>) -> Type {
        expr.ty_with_ctx(self)
    }
}
