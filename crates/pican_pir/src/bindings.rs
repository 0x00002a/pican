use std::collections::HashMap;

use pican_core::{
    ir::{Ident, IrNode},
    register::Register,
};

struct Binding<'a> {
    name: IrNode<Ident<'a>>,
    value: BindingValue<'a>,
}

pub struct Bindings<'a> {
    tbl: HashMap<IrNode<Ident<'a>>, BindingValue<'a>>,
}

impl<'a> Bindings<'a> {
    /// Define a new binding
    ///
    /// # Panics
    /// If the `name` already has a binding set
    pub fn define(&mut self, name: IrNode<Ident<'a>>, val: BindingValue<'a>) {
        let r = self.tbl.insert(name, val);
        assert!(r.is_none(), "a binding already exists for {name:#?}");
    }
    pub fn lookup(&self, name: IrNode<Ident<'a>>) -> Option<BindingValue<'a>> {
        self.tbl.get(&name).copied()
    }
}

#[derive(Clone, Copy)]
pub enum BindingValue<'a> {
    Constant(),
    SwizzleAlias(SwizzleAlias<'a>),
    Register(Register),
}

#[derive(Clone, Copy)]
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
