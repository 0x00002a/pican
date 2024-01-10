use super::ast::Stmt;
use crate::frontend::ast::{Block, FunctionDecl, Ident, Op, OpCode, Operand, Operands, Statement};
use crate::frontend::parse_ext::ParserExt;
use crate::ir::Float;
use crate::ir::{IrNode, Span};
use bumpalo::Bump;
use codespan::{FileId, Location};
use nom::bytes::complete::take_until;
use nom::character::complete::{self as nmc, space1};
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
        for (t, val) in tags {
            let (i_r, r) = ncm::opt(tag(*t)).parse(i)?;
            if r.is_some() {
                return Ok((i_r, *val));
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
    penum(&[("mov", Mov), ("dp4", Dp4), ("min", Min), ("mad", Mad)])
        .ctx("opcode")
        .parse(i)
}

fn operands<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operands<'a>> {
    fn operand<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operand<'a>> {
        nfo(ident).map(Operand::Var).parse(i)
    }
    let (i, ops) = nom::multi::separated_list1(tkn(","), nfo(operand))(i)?;
    let ops = i.extra.area.alloc_slice_copy(&ops);
    Ok((i, Operands(ops)))
}
fn op<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Op<'a>> {
    let (i, code) = nfo(opcode.ctx("opcode"))(i)?;
    let (i, _) = space1(i)?;
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
type PError<'a, I> = VerboseError<Input<'a, I>>;

fn run_parser<'a, 'p, O>(
    arena: &'a Bump,
    input: &'p str,
    file: FileId,
    mut parser: impl Parser<Input<'a, &'p str>, O, PError<'a, &'p str>>,
) -> Result<O, ErrorKind> {
    let input = LocatedSpan::new_extra(input, InputContext { area: arena, file });
    match parser.parse(input) {
        Ok((i, v)) => {
            if !i.is_empty() {
                Err(VerboseError {
                    errors: vec![(
                        i.into(),
                        VerboseErrorKind::Context("didn't consume all input"),
                    )],
                })
            } else {
                Ok(v)
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

pub fn parse<'a>(arena: &'a Bump, input: &str, file: FileId) -> Result<&'a [Stmt<'a>], ErrorKind> {
    run_parser(
        arena,
        input,
        file,
        many0_in(nfo(stmt)).map(|stmts| stmts.into_bump_slice()),
    )
}
#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use codespan::Files;
    use nom::{
        bytes::complete::tag,
        character::complete::{satisfy, space1},
        combinator::eof,
        Parser,
    };

    use crate::{
        frontend::ast::{OpCode, Stmt},
        ir::IrNode,
    };

    use super::{Input, PError};

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
        fn run_parser<'a, 'p, O>(
            &'a self,
            input: &'p str,
            parser: impl Parser<Input<'a, &'p str>, O, PError<'a, &'p str>>,
        ) -> Result<O, super::ErrorKind> {
            let mut db = Files::new();
            let id = db.add("test", input);
            super::run_parser(&self.arena, input, id, parser)
        }
    }

    #[test]
    fn parse_mov_op() {
        let ctx = TestCtx::new();
        let mov = ctx.run_parser("mov r1, r2", super::op).unwrap();
        assert_eq!(mov.opcode.get(), &OpCode::Mov);
        let operands = mov.operands.get().0;
        assert_eq!(operands.len(), 2);
        assert_eq!(operands[0].get().try_as_var().unwrap().get(), "r1");
    }
    #[test]
    fn parse_ident_with_num() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser("r1", super::ident).unwrap();
        assert_eq!(res, "r1");
    }

    #[test]
    fn parse_operands() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser("r1, r2", super::operands).unwrap();
        assert_eq!(res.0.len(), 2);
        assert_eq!(res.0[0].get().try_as_var().unwrap().get(), "r1");
        assert_eq!(res.0[1].get().try_as_var().unwrap().get(), "r2");
    }
    #[test]
    fn opcode_parser_doesnt_eat_space_before() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser(" mov", super::opcode);
        assert!(res.is_err());
    }

    #[test]
    fn opcode_parser_doesnt_eat_space_after() {
        let ctx = TestCtx::new();
        let _ = ctx
            .run_parser(
                "mov ",
                super::opcode.and(
                    satisfy(|ch| {
                        assert_eq!(ch, ' ');
                        true
                    })
                    .and(eof),
                ),
            )
            .unwrap();
    }
}
