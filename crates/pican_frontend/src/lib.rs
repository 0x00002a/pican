use std::borrow::Cow;

use lower::FrontendToPirCtx;
use pican_core::{
    alloc::Bump,
    context::{IrContext, PicanContext},
    diagnostics::DiagnosticBuilder,
    span::FileId,
};

pub mod ast;
pub mod lower;
pub mod parse;
mod parse_ext;

pub fn parse_and_lower<'a, S: AsRef<str>>(
    file: FileId,
    ctx: &PicanContext<S>,
    to: &'a IrContext,
) -> Option<pican_pir::ir::Module<'a>> {
    let arena = Bump::new();
    let ast = match parse::parse(&arena, ctx.files.source(file).as_ref(), file, ctx.opts) {
        Ok(ast) => ast,
        Err(e) => {
            let mut diag = DiagnosticBuilder::error();
            for (span, kind) in e.errors {
                let msg = match kind {
                    nom::error::VerboseErrorKind::Context(c) => Cow::Borrowed(c),
                    nom::error::VerboseErrorKind::Char(c) => Cow::Owned(format!("expected {c}")),
                    nom::error::VerboseErrorKind::Nom(n) => Cow::Owned(format!("in {n:#?}")),
                };
                diag = diag.note(&span, msg);
            }
            ctx.diag.add(diag.build());
            return None;
        }
    };
    let (m, e) = to.lower(ctx, ast);
    if e.is_some() {
        None
    } else {
        Some(m)
    }
}

pub trait PicanFrontendExt {
    fn parse_and_lower<'a>(
        &self,
        file: FileId,
        to: &'a IrContext,
    ) -> Option<pican_pir::ir::Module<'a>>;
}
impl<S: AsRef<str>> PicanFrontendExt for PicanContext<S> {
    fn parse_and_lower<'a>(
        &self,
        file: FileId,
        to: &'a IrContext,
    ) -> Option<pican_pir::ir::Module<'a>> {
        parse_and_lower(file, self, to)
    }
}
