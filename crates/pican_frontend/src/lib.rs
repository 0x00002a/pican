use lower::FrontendToPirCtx;
use pican_core::{
    alloc::Bump,
    context::{IrContext, PicanContext},
    diagnostics::{DiagnosticBuilder, FatalErrorEmitted},
    ir::IrNode,
    span::FileId,
};

pub mod ast;
pub mod lower;
pub mod parse;
mod parse_ext;

// todo: parse and lower needs to propagate lowering errors
pub fn parse_and_lower<'a, S: AsRef<str>>(
    file: FileId,
    ctx: &PicanContext<S>,
    to: &'a IrContext,
) -> Option<pican_pir::ir::Module<'a>> {
    let arena = Bump::new();
    let ast = match parse::parse(&arena, ctx.files.source(file).as_ref(), file) {
        Ok(ast) => ast,
        Err(e) => {
            for (span, kind) in e.errors {
                ctx.diag.add(
                    DiagnosticBuilder::error()
                        .at(&IrNode::new((), span))
                        .primary(format!("parse error {kind:?}"))
                        .build(),
                );
            }
            return None;
        }
    };
    Some(to.lower(ctx, ast))
}
