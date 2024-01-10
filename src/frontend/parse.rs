use super::ast::Stmt;
use crate::frontend::ast::{Block, FunctionDecl, Ident, Op, OpCode, Operand, Operands, Statement};
use crate::frontend::parse_ext::ParserExt;
use crate::ir::Float;
use crate::ir::{IrNode, Span};
use bumpalo::Bump;
use codespan::{FileId, Location};
use nom::bytes::complete::take_until;
use nom::character::complete as nmc;
use nom::error::context;
use nom::multi::{fold_many0, many0, many0_count, many1, many_till};
use nom::sequence::{pair, preceded, separated_pair, terminated, tuple};
use nom::{
    branch::{self},
    bytes::complete::tag,
    error::ParseError,
    sequence::delimited,
    IResult,
};
use nom::{combinator as ncm, FindSubstring, InputIter};
use nom::{
    error::{VerboseError, VerboseErrorKind},
    Parser,
};
use nom::{AsChar, Compare, InputLength, InputTake, InputTakeAtPosition};
use nom_locate::LocatedSpan;
use std::rc::Rc;

#[derive(Clone, Copy)]
struct InputContext<'a> {
    area: &'a bumpalo::Bump,
    file: FileId,
}
impl<'a> InputContext<'a> {
    pub fn alloc<A: Copy>(&self, val: A) -> &'a mut A {
        self.area.alloc(val)
    }
    pub fn alloc_slice<A: Copy>(&self, val: &[A]) -> &'a mut [A] {
        self.area.alloc_slice_copy(val)
    }
    pub fn span(&self, start: usize, end: usize) -> Span {
        Span::new(
            start.try_into().expect("start is too large"),
            end.try_into().expect("end is too large"),
            self.file,
        )
    }
}

type Nfo<T> = IrNode<T>;

type Pres<'a, 'p, T, I = Input<'a, &'p str>, E = nom::error::VerboseError<I>> = IResult<I, T, E>;
type NfoRes<'a, 'p, T> = Pres<'a, 'p, Nfo<T>>;
pub type Input<'a, T> = LocatedSpan<T, InputContext<'a>>;

macro_rules! or_kw {
        ($($c:ident),* $(,)?) => {
            ::nom::combinator::fail $(.or(tag(stringify!($c))))*
        }
    }

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, I: 'a, F: 'a, O: 'a, E: ParseError<I> + 'a>(
    inner: F,
) -> impl FnMut(I) -> IResult<I, O, E> + 'a
where
    F: Fn(I) -> IResult<I, O, E>,
    I: InputTakeAtPosition
        + Clone
        + InputLength
        + InputTake
        + FindSubstring<&'a str>
        + Compare<&'a str>
        + InputIter,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(space, inner, space)
}

fn space<'a, I, E>(i: I) -> IResult<I, (), E>
where
    E: ParseError<I>,
    I: Clone
        + InputLength
        + InputTakeAtPosition
        + InputTake
        + FindSubstring<&'a str>
        + Compare<&'a str>
        + InputIter,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    many0_count(nmc::multispace1.map(|_| ()).or(comment))
        .map(|_| ())
        .parse(i)
}
fn comment<'a, I, E>(i: I) -> IResult<I, (), E>
where
    E: ParseError<I>,
    I: InputTakeAtPosition
        + InputTake
        + Clone
        + FindSubstring<&'a str>
        + InputLength
        + InputIter
        + Compare<&'a str>,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    let (i, _) = preceded(tag("--[["), take_until("]]--"))
        .or(preceded(tag("--"), take_until("\n")))
        .parse(i)?;
    Ok((i, ()))
}
fn tkn<'a, I: 'a, T: 'a, E: ParseError<I> + 'a>(ch: T) -> impl FnMut(I) -> IResult<I, I, E> + 'a
where
    I: InputTakeAtPosition
        + Compare<T>
        + InputTake
        + Clone
        + InputLength
        + FindSubstring<&'a str>
        + InputIter
        + Compare<&'a str>,
    T: InputLength + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    ws(tag(ch))
}
pub fn many0_in<'a, T: Clone + InputLength, O, E: ParseError<Input<'a, T>>>(
    mut f: impl Parser<Input<'a, T>, O, E>,
) -> impl Parser<Input<'a, T>, bumpalo::collections::Vec<'a, O>, E> {
    move |i: Input<'a, T>| {
        let f = |i| f.parse(i);
        fold_many0(
            f,
            || bumpalo::collections::Vec::new_in(i.extra.area),
            |mut acc, item| {
                acc.push(item);
                acc
            },
        )
        .parse(i)
    }
}

fn penum<'a, 'p, 't: 'p, I: 'p, E: Copy, Error: ParseError<I> + 'p>(
    tags: &'t [(&str, E)],
) -> impl Parser<I, E, Error> + 't
where
    I: InputTakeAtPosition
        + InputTake
        + Clone
        + Copy
        + InputLength
        + FindSubstring<&'p str>
        + InputIter
        + Compare<&'p str>,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    move |mut i| {
        for (tag, val) in tags {
            let (i_r, r) = ncm::opt(tkn::<'p, I, _, Error>(*tag)).parse(i)?;
            if r.is_some() {
                return Ok((i, *val));
            } else {
                i = i_r;
            }
        }
        ncm::fail(i)
    }
}

fn lift<I, O, Ot, E: ParseError<I>, F>(inner: F) -> impl FnMut(I) -> IResult<I, Nfo<Ot>, E>
where
    F: Fn(I) -> IResult<I, Nfo<O>, E>,
    Ot: From<O>,
{
    ncm::map(inner, |n| n.lift())
}
pub fn nfo<'a, I, O, E: ParseError<Input<'a, I>>, F>(
    mut inner: F,
) -> impl FnMut(Input<'a, I>) -> IResult<Input<'a, I>, Nfo<O>, E>
where
    F: Parser<Input<'a, I>, O, E>,
{
    move |i| {
        let start = i.location_offset();
        let (i, v) = inner.parse(i)?;
        let ctx = i.extra;
        let end = i.location_offset();
        let span = ctx.span(start, end);
        Ok((i, Nfo::new(v, span)))
    }
}

fn value<'a, I, O: Clone, Oe, E: ParseError<Input<'a, I>>, F>(
    val: O,
    inner: F,
) -> impl FnMut(Input<'a, I>) -> IResult<Input<'a, I>, Nfo<O>, E>
where
    F: Parser<Input<'a, I>, Oe, E>,
{
    nfo(ncm::value(val, inner))
}

pub fn stmt<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Statement<'a>> {
    nfo(op).map(Into::into).parse(i)
}

fn array_index<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Nfo<u8>> {
    tkn("[")
        .ignore_then(nfo(nmc::u8).req("expected index"))
        .then_ignore(tkn("]").req("missing closing ]"))
        .parse(i)
}
fn ident_word<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, &'a str> {
    fn ident_char<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, char> {
        branch::alt((
            nmc::satisfy(|ch| ch.is_alpha()),
            ncm::value('_', nmc::char('_')),
        ))
        .parse(i)
    }
    let (i, _) = ncm::peek(ident_char)(i)?;
    let (i, word) = many1(ident_char.or(nmc::satisfy(|ch| ch.is_dec_digit())))(i)?;
    let word = bumpalo::collections::String::from_iter_in(word.into_iter(), i.extra.area);
    Ok((i, word.into_bump_str()))
}
pub fn ident<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Ident<'a>> {
    let keywords = or_kw! {
        mov, dp4
    };
    let not_kw = ncm::not(keywords);
    not_kw.ignore_then(ident_word).map(Ident::new).parse(i)
}
fn f32<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, f32> {
    let (i, (ipart, _, floating)) =
        tuple((nmc::i32, tag("."), nmc::u64.req("missing floating part"))).parse(i)?;
    Ok((i, (ipart as f32) + 1. / (floating as f32)))
}
fn float<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Float> {
    let (i, val) = branch::alt((f32, ncm::map(nmc::i64, |n| n as f32)))
        .ctx("float")
        .parse(i)?;
    if let Some(f) = Float::new(val) {
        Ok((i, f))
    } else {
        ncm::fail(i)
    }
}
fn entry_point<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, FunctionDecl<'a>> {
    pub fn block<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Block<'a>> {
        let (i, info) = nfo(many_till(nfo(stmt), tkn(".end").ctx("block end")))(i)?;
        let statements = info.map(|(val, _)| i.extra.alloc_slice(&val)).map(|v| &*v);
        Ok((i, Block { statements }))
    }

    let (i, name) = nfo(tag(".").ignore_then(ident.req("missing identifier after '.'")))(i)?;
    let (i, block) = nfo(block.req("missing block end"))(i)?;
    Ok((i, FunctionDecl { name, block }))
}

fn opcode<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, OpCode> {
    use OpCode::*;
    penum(&[("mov", Mov), ("dp4", Dp4), ("min", Min), ("mad", Mad)]).parse(i)
}
fn op<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Op<'a>> {
    fn operands<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operands<'a>> {
        fn operand<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operand<'a>> {
            nfo(ident).map(Operand::Var).parse(i)
        }
        let (i, ops) = nom::multi::separated_list1(tkn(","), nfo(operand))(i)?;
        let ops = i.extra.area.alloc_slice_copy(&ops);
        Ok((i, Operands(ops)))
    }
    let (i, code) = nfo(opcode.ctx("opcode"))(i)?;
    let (i, operands) = nfo(operands.req("missing operands to operation"))(i)?;
    Ok((
        i,
        Op {
            opcode: code,
            operands,
        },
    ))
}

pub type ErrorKind = nom::error::VerboseError<Span>;
impl<'a, T> From<Input<'a, T>> for Span {
    fn from(value: Input<'a, T>) -> Self {
        value
            .extra
            .span(value.location_offset(), value.location_offset())
    }
}

pub fn parse<'a>(arena: &'a Bump, input: &str, file: FileId) -> Result<&'a [Stmt<'a>], ErrorKind> {
    let input = LocatedSpan::new_extra(input, InputContext { area: arena, file });
    match many0_in(nfo(stmt)).parse(input) {
        Ok((i, v)) => {
            if !i.is_empty() {
                Err(VerboseError {
                    errors: vec![(
                        i.into(),
                        VerboseErrorKind::Context("didn't consume all input"),
                    )],
                })
            } else {
                Ok(v.into_bump_slice())
            }
        }
        Err(e) => match e.map(|v| VerboseError {
            errors: v
                .errors
                .into_iter()
                //  .filter(|e| matches!(e.1, VerboseErrorKind::Context(_)))
                .map(|i| (i.0.into(), i.1))
                .collect(),
        }) {
            nom::Err::Error(e) => Err(e),
            nom::Err::Failure(f) => Err(f),
            nom::Err::Incomplete(_) => todo!(),
        },
    }
}
#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use codespan::Files;

    use crate::{
        frontend::ast::{OpCode, Stmt},
        ir::IrNode,
    };

    struct TestCtx {
        arena: Bump,
    }
    impl TestCtx {
        fn new() -> Self {
            Self { arena: Bump::new() }
        }

        fn parse<'a>(&'a self, input: &str) -> Result<&'a [Stmt<'a>], super::ErrorKind> {
            let mut db = Files::new();
            let id = db.add("test", input);
            super::parse(&self.arena, input, id)
        }
    }

    #[test]
    fn parse_mov_op() {
        let ctx = TestCtx::new();
        let res = ctx.parse("mov r1, r2").unwrap();
        assert_eq!(res.len(), 1);
        let stmt = res[0];
        let mov = stmt.into_inner().try_into_op().unwrap().into_inner();
        assert_eq!(mov.opcode.get(), &OpCode::Mov);
        let operands = mov.operands.get().0;
        assert_eq!(operands.len(), 2);
        assert_eq!(operands[0].get().try_as_var().unwrap().get(), "r1");
    }
}
