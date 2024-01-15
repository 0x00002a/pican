use diagnostics::FatalErrorEmitted;

pub mod alloc;
pub mod context;
pub mod copy_arrayvec;
pub mod diagnostics;
pub mod ir;
pub mod ops;
pub mod properties;
pub mod register;
pub mod span;

pub enum PError<E> {
    DiagnosticEmitted(FatalErrorEmitted),
    Error(E),
}

pub type PResult<T, E> = std::result::Result<T, PError<E>>;
