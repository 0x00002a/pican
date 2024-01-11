use std::collections::HashMap;

use pican_core::{
    ir::{Ident, IrNode, Span},
    register::Register,
};
use serde::Serialize;

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, Hash)]
struct Binding<'a> {
    name: IrNode<Ident<'a>>,
    value: IrNode<BindingValue<'a>>,
}

#[derive(Debug, Default, Serialize, PartialEq, Eq, Clone)]
pub struct Bindings<'a> {
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

    pub fn lookup(&self, name: IrNode<Ident>) -> Option<IrNode<BindingValue<'a>>> {
        if let Some(prev) = self.previous_definition(name) {
            assert_eq!(
                prev, name,
                "an identifier different with a conflicting name already existed when doing lookup"
            );
        }
        self.tbl.get(name.get()).map(|b| b.value)
    }
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
#[typesum::sumtype(only = from)]
pub enum BindingValue<'a> {
    SwizzleAlias(SwizzleAlias<'a>),
    Register(Register),
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
pub struct SwizzleAlias<'a> {
    pub target: IrNode<Ident<'a>>,
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
