use std::collections::HashMap;

use pican_core::{
    diagnostics::{DiagnosticBuilder, Diagnostics, FatalErrorEmitted},
    ir::{Ident, IrNode, SwizzleDims},
    register::Register,
};
use serde::Serialize;

use crate::ir::{ConstantUniform, InputBinding, OutputBinding, Uniform};

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, Hash)]
struct Binding<'a> {
    name: IrNode<Ident<'a>>,
    value: IrNode<BindingValue<'a>>,
}

#[derive(Debug, Default, Serialize, PartialEq, Eq, Clone)]
pub struct Bindings<'a> {
    #[serde(flatten)]
    tbl: HashMap<Ident<'a>, Binding<'a>>,
}

impl<'a> Bindings<'a> {
    /// Define a new binding
    ///
    /// # Panics
    /// If the `name` already has a binding set
    pub fn define(&mut self, name: IrNode<Ident<'a>>, val: IrNode<impl Into<BindingValue<'a>>>) {
        let r = self.tbl.insert(
            name.into_inner(),
            Binding {
                name,
                value: val.map(Into::into),
            },
        );
        assert!(r.is_none(), "a binding already exists for {name:#?}");
    }
    pub fn previous_definition<'b>(&self, name: IrNode<Ident<'b>>) -> Option<IrNode<Ident<'a>>> {
        self.tbl.get(name.get()).map(|Binding { name, .. }| *name)
    }

    pub fn lookup(&self, name: &Ident) -> Option<IrNode<BindingValue<'a>>> {
        self.tbl.get(name).map(|b| b.value)
    }
    pub fn lookup_with_diag(
        &self,
        name: IrNode<Ident>,
        diag: &Diagnostics,
    ) -> Result<IrNode<BindingValue<'a>>, FatalErrorEmitted> {
        self.lookup(name.get()).ok_or_else(|| {
            diag.fatal::<()>(
                DiagnosticBuilder::error()
                    .at(&name)
                    .primary("cannot find binding for identifier")
                    .build(),
            )
            .unwrap_err()
        })
    }
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
#[typesum::sumtype(only = from)]
#[serde(rename_all = "snake_case", tag = "ty", content = "value")]
pub enum BindingValue<'a> {
    SwizzleRegister(SwizzleValue<'a, Register>),
    SwizzleVar(SwizzleValue<'a, Ident<'a>>),
    Alias(Ident<'a>),
    Register(Register),
    Uniform(Uniform),
    Constant(ConstantUniform<'a>),
    OutputProperty(&'a OutputBinding<'a>),
    Input(&'a InputBinding<'a>),
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
pub struct SwizzleValue<'a, T> {
    pub target: IrNode<T>,
    pub swizzle: IrNode<SwizzleDims<'a>>,
}

pub struct Alias<'a> {
    pub kind: IrNode<AliasKind>,
    pub from: IrNode<Ident<'a>>,
}

pub enum AliasKind {
    Output,
    Input,
    Var,
}
