use diagnostics::FatalErrorEmitted;

pub mod alloc;
pub mod asm;
pub mod context;
pub mod copy_arrayvec;
pub mod diagnostics;
pub mod frontend;
pub mod ir;
pub mod ops;
pub mod pir;
pub mod properties;
pub mod register;
pub mod span;
pub mod ty;

pub enum PError<E> {
    DiagnosticEmitted(FatalErrorEmitted),
    Error(E),
}

pub type PResult<T, E> = std::result::Result<T, PError<E>>;
