pub struct Bindings<'a> {}

pub struct Alias<'a> {
    pub kind: IrNode<AliasKind>,
    pub from: IrNode<Ident<'a>>,
}

pub enum AliasKind {
    Output,
    Input,
    Var,
}
