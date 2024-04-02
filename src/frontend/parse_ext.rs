use std::marker::PhantomData;

use nom::{
    error::{ContextError, ParseError},
    Parser,
};

pub struct WithContext<P> {
    ctx: &'static str,
    inner: P,
}
impl<I, O, E, P> Parser<I, O, E> for WithContext<P>
where
    P: Parser<I, O, E>,
    I: Clone,
    E: ContextError<I>,
{
    fn parse(&mut self, input: I) -> nom::IResult<I, O, E> {
        nom::error::context(self.ctx, |i| self.inner.parse(i)).parse(input)
    }
}
pub struct Cut<P> {
    inner: P,
}
impl<I, O, E, P> Parser<I, O, E> for Cut<P>
where
    P: Parser<I, O, E>,
    E: ParseError<I>,
{
    fn parse(&mut self, input: I) -> nom::IResult<I, O, E> {
        nom::combinator::cut(|i| self.inner.parse(i)).parse(input)
    }
}
pub struct IgnoreThen<I, K, O2> {
    ignored: I,
    keep: K,
    inter: PhantomData<O2>,
}
impl<I, O1, O2, K, E, P> Parser<I, O1, E> for IgnoreThen<P, K, O2>
where
    P: Parser<I, O2, E>,
    K: Parser<I, O1, E>,
    E: ParseError<I>,
{
    fn parse(&mut self, i: I) -> nom::IResult<I, O1, E> {
        let (i, _) = self.ignored.parse(i)?;
        let (i, v) = self.keep.parse(i)?;
        Ok((i, v))
    }
}
pub struct ThenIgnore<I, K, O2> {
    ignored: I,
    keep: K,
    inter: PhantomData<O2>,
}
impl<I, O1, O2, K, E, P> Parser<I, O1, E> for ThenIgnore<P, K, O2>
where
    P: Parser<I, O2, E>,
    K: Parser<I, O1, E>,
    E: ParseError<I>,
{
    fn parse(&mut self, i: I) -> nom::IResult<I, O1, E> {
        let (i, v) = self.keep.parse(i)?;
        let (i, _) = self.ignored.parse(i)?;
        Ok((i, v))
    }
}

pub trait ParserExt<I, O, E>: Parser<I, O, E> + Sized {
    /// Add context to parser, used in errors
    fn ctx(self, lbl: &'static str) -> WithContext<Self>
    where
        I: Clone,
    {
        WithContext {
            ctx: lbl,
            inner: self,
        }
    }

    /// Require this parser to succeed and provide context for if it fails
    fn req(self, msg: &'static str) -> WithContext<Cut<Self>>
    where
        E: ParseError<I>,
        I: Clone,
    {
        self.cut().ctx(msg)
    }

    fn cut(self) -> Cut<Self> {
        Cut { inner: self }
    }
    fn ignore_then<Op, P>(self, p: P) -> IgnoreThen<Self, P, O>
    where
        P: Parser<I, Op, E>,
    {
        IgnoreThen {
            ignored: self,
            keep: p,
            inter: PhantomData,
        }
    }

    fn then_ignore<Op, P>(self, p: P) -> ThenIgnore<P, Self, Op>
    where
        P: Parser<I, Op, E>,
    {
        ThenIgnore {
            ignored: p,
            keep: self,
            inter: PhantomData,
        }
    }
}

impl<I, O, E, P> ParserExt<I, O, E> for P where P: Parser<I, O, E> {}
