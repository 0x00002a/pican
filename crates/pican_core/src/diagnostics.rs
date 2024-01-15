use std::{borrow::Cow, cell::RefCell, io::Write};

use codespan::FileId;
use codespan_reporting::diagnostic::{Label, LabelStyle, Severity};
use serde::Serialize;

use crate::ir::{HasSpan, IrNode, Span};

#[derive(Debug, Serialize)]
pub struct Diagnostic {
    level: DiagnosticLevel,
    pub messages: Box<[DiagnosticMessage]>,
}

pub struct FatalErrorEmitted(());

#[derive(Debug, Serialize)]
pub struct DiagnosticMessage {
    style: LabelStyle,
    txt: Cow<'static, str>,
    span: Option<Span>,
    severity: Severity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
enum DiagnosticLevel {
    Fatal,
    Error,
    Warn,
}
impl From<DiagnosticLevel> for Severity {
    fn from(val: DiagnosticLevel) -> Self {
        match val {
            DiagnosticLevel::Fatal | DiagnosticLevel::Error => Severity::Error,
            DiagnosticLevel::Warn => Severity::Warning,
        }
    }
}

pub struct DiagnosticBuilder {
    level: DiagnosticLevel,
    messages: Vec<DiagnosticMessage>,
    primary_loc: Option<Span>,
}
impl DiagnosticBuilder {
    fn with_level(level: DiagnosticLevel) -> Self {
        Self {
            level,
            messages: Default::default(),
            primary_loc: None,
        }
    }

    pub fn note(mut self, node: &impl HasSpan, msg: impl Into<Cow<'static, str>>) -> Self {
        self.messages.push(DiagnosticMessage {
            style: LabelStyle::Secondary,
            txt: msg.into(),
            span: Some(node.span()),
            severity: Severity::Note,
        });
        self
    }
    pub fn at(mut self, node: &impl HasSpan) -> Self {
        self.primary_loc.replace(node.span());
        self
    }
    pub fn primary(mut self, msg: impl Into<Cow<'static, str>>) -> Self {
        self.messages.push(DiagnosticMessage {
            style: LabelStyle::Primary,
            txt: msg.into(),
            span: self.primary_loc,
            severity: self.level.into(),
        });
        self
    }

    pub fn error() -> Self {
        Self::with_level(DiagnosticLevel::Error)
    }

    pub fn warn() -> Self {
        Self::with_level(DiagnosticLevel::Warn)
    }
    pub fn build(self) -> Diagnostic {
        Diagnostic {
            level: self.level,
            messages: self.messages.into_boxed_slice(),
        }
    }
}

#[derive(Default, Debug)]
pub struct Diagnostics {
    diags: RefCell<Vec<Diagnostic>>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn fatal<T>(&self, mut diag: Diagnostic) -> Result<T, FatalErrorEmitted> {
        diag.level = DiagnosticLevel::Fatal;
        self.add(diag);
        Err(FatalErrorEmitted(()))
    }

    pub fn add(&self, diag: Diagnostic) {
        self.diags.borrow_mut().push(diag)
    }
    pub fn as_codespan(&self) -> Vec<codespan_reporting::diagnostic::Diagnostic<FileId>> {
        let mut reporting_diags = Vec::new();
        for d in self.diags.borrow().iter() {
            let mut diag = codespan_reporting::diagnostic::Diagnostic::new(d.level.into());
            for DiagnosticMessage {
                style,
                txt,
                span,
                severity,
            } in d.messages.iter()
            {
                if let Some(span) = span {
                    let lbl = Label::new(
                        *style,
                        span.file(),
                        span.src_span().start().0 as usize..span.src_span().end().0 as usize,
                    );
                    diag.labels.push(lbl);
                }
                match style {
                    LabelStyle::Primary => diag.message = txt.clone().into_owned(),
                    LabelStyle::Secondary => {
                        let mut diag = codespan_reporting::diagnostic::Diagnostic::new(*severity)
                            .with_message(txt.clone());

                        if let Some(span) = span {
                            let lbl = Label::new(
                                LabelStyle::Primary,
                                span.file(),
                                span.src_span().start().0 as usize
                                    ..span.src_span().end().0 as usize,
                            );
                            diag.labels.push(lbl);
                        }
                        reporting_diags.push(diag);
                    }
                }
            }
            reporting_diags.push(diag);
        }
        reporting_diags
    }
}
