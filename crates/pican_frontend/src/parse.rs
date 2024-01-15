use super::ast::Stmt;
use crate::ast::{
    self, Block, FunctionDecl, Ident, Op, OpCode, Operand, Operands, RegisterBind, Statement,
    Uniform, UniformDecl, UniformTy,
};
use crate::parse_ext::ParserExt;
use nom::bytes::complete::take_until;
use nom::character::complete::{self as nmc, line_ending, not_line_ending, space1};
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
use nom::{combinator as ncm, FindSubstring, InputIter, Offset, Slice};
use nom::{
    error::{VerboseError, VerboseErrorKind},
    Parser,
};
use nom::{AsChar, Compare, InputLength, InputTake, InputTakeAtPosition};
use nom_locate::LocatedSpan;
use pican_core::alloc::Bump;
use pican_core::ir::Float;
use pican_core::ir::{IrNode, Span};
use pican_core::register::{Register, RegisterKind};
use pican_core::span::{FileId, Location};
use std::rc::Rc;

#[derive(Clone, Copy)]
struct InputContext<'a> {
    area: &'a Bump,
    file: FileId,
}
impl<'a> InputContext<'a> {
    pub fn alloc<A: Copy>(&self, val: A) -> &'a mut A {
        self.area.alloc(val)
    }
    pub fn alloc_slice<A: Copy>(&self, val: &[A]) -> &'a mut [A] {
        self.area.alloc_slice_copy(val)
    }

    pub fn alloc_str(&self, val: &str) -> &'a str {
        self.area.alloc_str(val)
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
        + nom::Slice<std::ops::RangeFrom<usize>>
        + InputIter
        + Offset
        + nom::Slice<std::ops::RangeTo<usize>>,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as nom::InputIter>::Item: nom::AsChar,
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
        + InputIter
        + nom::Slice<std::ops::RangeFrom<usize>>
        + nom::Offset
        + nom::Slice<std::ops::RangeTo<usize>>,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as nom::InputIter>::Item: nom::AsChar,
{
    many0_count(nmc::multispace1.map(|_| ()).or(comment.map(|_| ())))
        .map(|_| ())
        .parse(i)
}
fn comment<'a, I, E>(i: I) -> IResult<I, I, E>
where
    E: ParseError<I>,
    I: InputTakeAtPosition
        + InputTake
        + Clone
        + FindSubstring<&'a str>
        + InputLength
        + InputIter
        + Compare<&'a str>
        + Offset
        + nom::Slice<std::ops::RangeFrom<usize>>
        + nom::Slice<std::ops::RangeTo<usize>>,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as nom::InputIter>::Item: nom::AsChar,
{
    let (i, c) = tag(";")
        .ignore_then(ncm::recognize(many0_count(
            ncm::not(tag("\n").or(ncm::eof)).ignore_then(nmc::anychar),
        )))
        .then_ignore(ncm::opt(tag("\n")))
        .parse(i)?;
    Ok((i, c))
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
        + nom::Slice<std::ops::RangeFrom<usize>>
        + Offset
        + nom::Slice<std::ops::RangeTo<usize>>
        + Compare<&'a str>,
    T: InputLength + Clone,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    <I as nom::InputIter>::Item: nom::AsChar,
{
    ws(tag(ch))
}
pub fn many0_in<'a, T: Clone + InputLength, O, E: ParseError<Input<'a, T>>>(
    mut f: impl Parser<Input<'a, T>, O, E>,
) -> impl Parser<Input<'a, T>, pican_core::alloc::collections::Vec<'a, O>, E> {
    move |i: Input<'a, T>| {
        let f = |i| f.parse(i);
        fold_many0(
            f,
            || pican_core::alloc::collections::Vec::new_in(i.extra.area),
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
    branch::alt((
        nfo(register_bind).map(Into::into),
        nfo(uniform_decl).map(Into::into),
        nfo(comment)
            .map(|c| {
                c.map(|s: Input<'a, &'p str>| i.extra.alloc_str(s.fragment()))
                    .map(ast::Comment)
            })
            .map(Statement::Comment),
        nfo(op).map(Into::into),
        nfo(entry_point).map(Into::into),
    ))
    .parse(i)
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
    let word = pican_core::alloc::collections::String::from_iter_in(word.into_iter(), i.extra.area);
    Ok((i, word.into_bump_str()))
}
fn register<'a, 'p>(mut i: Input<'a, &'p str>) -> Pres<'a, 'p, Register> {
    for reg_kind in RegisterKind::all() {
        let p = nmc::char(reg_kind.prefix()).ignore_then(nmc::u32.req("register index"));
        let (i_r, r) = ncm::opt(p)(i)?;
        i = i_r;
        if let Some(index) = r {
            return Ok((
                i,
                Register {
                    kind: reg_kind,
                    index: index.try_into().expect("index too large for usize"),
                },
            ));
        }
    }
    ncm::fail(i)
}
pub fn register_bind<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, RegisterBind<'a>> {
    let (i, _) = tag(".alias")(i)?;
    let (i, _) = space(i)?;
    let (i, name) = nfo(ident.req("expected identifier for alias"))(i)?;
    let (i, _) = space(i)?;
    let (i, reg) = nfo(register.req("expected register for alias"))(i)?;
    Ok((i, RegisterBind { name, reg }))
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

fn uniform_ty<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, UniformTy> {
    penum(&[
        ("fvec", UniformTy::Float),
        ("ivec", UniformTy::Integer),
        ("bool", UniformTy::Bool),
    ])
    .parse(i)
}
fn uniform<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Uniform<'a>> {
    fn dims<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, u8> {
        tag("[")
            .ignore_then(nmc::u8.req("missing size in uniform dims"))
            .then_ignore(tkn("]").req("missing closing ] for uniform dimensions"))
            .parse(i)
    }
    let (i, name) = nfo(ident)(i)?;
    let (i, dimensions) = ncm::opt(nfo(dims))(i)?;
    Ok((i, Uniform { name, dimensions }))
}

fn uniform_decl<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, UniformDecl<'a>> {
    let (i, _) = tag(".")(i)?;
    let (i, ty) = nfo(uniform_ty)(i)?;
    let (i, _) = space(i)?;
    let (i, uniforms) = nfo(nom::multi::separated_list1(tkn(","), nfo(uniform)))
        .req("expected uniform name")
        .parse(i)?;
    let uniforms = uniforms.map(|u| -> &'a _ { i.extra.alloc_slice(&u) });
    Ok((i, UniformDecl { ty, uniforms }))
}
fn entry_point<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, FunctionDecl<'a>> {
    pub fn block<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Block<'a>> {
        let (i, _) = space(i)?; // consume whitespace so its part of our span and not the first statement's
        let (i, info) = nfo(many_till(nfo(stmt), tkn(".end").ctx("block end")))(i)?;
        let statements = info.map(|(val, _)| i.extra.alloc_slice(&val)).map(|v| &*v);
        Ok((i, Block { statements }))
    }

    let (i, name) = nfo(tag(".proc ").ignore_then(ident.req("missing identifier after '.'")))(i)?;
    let (i, block) = nfo(block).req("missing block end").parse(i)?;
    Ok((i, FunctionDecl { name, block }))
}

fn opcode<'a, 'p>(mut i: Input<'a, &'p str>) -> Pres<'a, 'p, OpCode> {
    for (name, val) in OpCode::variants_lookup() {
        let (i_r, ok) = ncm::opt(tag(name))(i)?;
        if ok.is_some() {
            return Ok((i_r, val));
        } else {
            i = i_r
        }
    }
    ncm::fail(i)
}

fn operands<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operands<'a>> {
    fn operand<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operand<'a>> {
        branch::alt((
            nfo(register).map(Operand::Register),
            nfo(ident).map(Operand::Var),
        ))
        .parse(i)
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

fn input_span<'a, T>(value: Input<'a, T>) -> Span {
    value
        .extra
        .span(value.location_offset(), value.location_offset())
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
                        input_span(i),
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
                .map(|i| (input_span(i.0), i.1))
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
    use std::str::FromStr;

    use nom::{
        bytes::complete::tag,
        character::complete::{satisfy, space1},
        combinator::eof,
        Parser,
    };
    use pican_core::ir::IrNode;
    use pican_core::span::Files;
    use pican_core::{
        alloc::Bump,
        register::{Register, RegisterKind},
    };

    use crate::ast::{OpCode, Stmt, UniformTy};

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
        let mov = ctx.run_parser("mov dst, r2", super::op).unwrap();
        assert_eq!(mov.opcode.get(), &OpCode::Mov);
        let operands = mov.operands.get().0;
        assert_eq!(operands.len(), 2);
        assert_eq!(operands[0].get().try_as_var().unwrap().get(), "dst");
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
        let res = ctx.run_parser("smth, smth2", super::operands).unwrap();
        assert_eq!(res.0.len(), 2);
        assert_eq!(res.0[0].get().try_as_var().unwrap().get(), "smth");
        assert_eq!(res.0[1].get().try_as_var().unwrap().get(), "smth2");
    }
    #[test]
    fn opcode_parser_doesnt_eat_space_before() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser(" mov", super::opcode);
        assert!(res.is_err());
    }

    #[test]
    fn operands_can_be_registers() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser("v0, r1", super::operands).unwrap();
        assert_eq!(
            res.0[0].get().try_as_register().unwrap().get(),
            &Register::from_str("v0").unwrap(),
        );

        assert_eq!(
            res.0[1].get().try_as_register().unwrap().get(),
            &Register::from_str("r1").unwrap(),
        );
    }
    #[test]
    fn parse_registers() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser("v0", super::register).unwrap();
        assert_eq!(res, Register::new(RegisterKind::Input, 0));
    }
    #[test]
    fn parse_comment() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("; sup", super::comment).unwrap();
        assert_eq!(r.fragment(), &" sup");
    }
    #[test]
    fn parse_comment_newline() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("; sup\n", super::comment).unwrap();
        assert_eq!(r.fragment(), &" sup");
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
    #[test]
    fn parse_entry_point() {
        let ctx = TestCtx::new();
        let ep = ctx.run_parser(".proc m .end", super::entry_point).unwrap();
        assert_eq!(ep.name.get(), "m")
    }

    #[test]
    fn parse_uniform() {
        let ctx = TestCtx::new();
        let u = ctx.run_parser("m[5]", super::uniform).unwrap();
        assert_eq!(u.name.get(), "m");
        assert_eq!(u.dimensions.unwrap().get(), &5);
    }
    #[test]
    fn parse_fvec_uniform() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser(".fvec m[5]", super::uniform_decl).unwrap();
        assert_eq!(r.ty.get(), &UniformTy::Float);
        assert_eq!(r.uniforms.get().len(), 1);
        let u = &r.uniforms.get()[0].get();
        assert_eq!(u.name.get(), "m");
        assert_eq!(u.dimensions.unwrap().get(), &5);
    }
    #[test]
    fn parse_alias() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser(".alias m r0", super::register_bind).unwrap();
        assert_eq!(r.name.get(), "m");
        assert_eq!(r.reg.get(), &Register::from_str("r0").unwrap());
    }
}
