use std::{borrow::Cow, cell::RefCell};

use codespan_reporting::diagnostic::{LabelStyle, Severity};

use crate::ir::{IrNode, Span};

pub struct Diagnostic {
    level: DiagnosticLevel,
    pub messages: Box<[DiagnosticMessage]>,
}

pub struct FatalErrorEmitted(());

pub struct DiagnosticMessage {
    style: LabelStyle,
    txt: Cow<'static, str>,
    span: Option<Span>,
    severity: Severity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

    pub fn note<T>(mut self, node: &IrNode<T>, msg: impl Into<Cow<'static, str>>) -> Self {
        self.messages.push(DiagnosticMessage {
            style: LabelStyle::Secondary,
            txt: msg.into(),
            span: Some(node.span()),
            severity: Severity::Note,
        });
        self
    }
    pub fn at<T>(mut self, node: &IrNode<T>) -> Self {
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

#[derive(Default)]
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
}
