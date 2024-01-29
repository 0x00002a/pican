use super::ast::Stmt;
use crate::ast::{
    self, Block, Constant, ConstantDecl, ConstantDiscriminants, Directive, FunctionDecl, Ident,
    InputBind, Op, OpCode, Operand, OperandKind, Operands, OutputBind, RegisterBind,
    RegisterBindTarget, Statement, SwizzleExpr, Uniform, UniformDecl, UniformTy,
};
use crate::parse_ext::ParserExt;

use nom::character::complete::{self as nmc};

use nom::multi::{fold_many0, many0_count, many1, many_till};
use nom::sequence::tuple;
use nom::{
    branch::{self},
    bytes::complete::tag,
    error::ParseError,
    sequence::delimited,
    IResult,
};
use nom::{combinator as ncm, FindSubstring, InputIter, Offset};
use nom::{
    error::{VerboseError, VerboseErrorKind},
    Parser,
};
use nom::{AsChar, Compare, InputLength, InputTake, InputTakeAtPosition};
use nom_locate::LocatedSpan;
use pican_core::alloc::Bump;
use pican_core::ir::{Float, HasSpan, SwizzleDim, SwizzleDims};
use pican_core::ir::{IrNode, Span};
use pican_core::properties::OutputProperty;
use pican_core::register::{Register, RegisterKind};
use pican_core::span::FileId;

#[derive(Clone, Copy)]
struct InputContext<'a> {
    area: &'a Bump,
    file: FileId,
}
impl<'a> InputContext<'a> {
    #[allow(unused)]
    pub fn alloc<A: Copy>(&self, val: A) -> &'a A {
        self.area.alloc(val)
    }

    pub fn alloc_slice<A: Copy>(&self, val: &[A]) -> &'a [A] {
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
type Input<'a, T> = LocatedSpan<T, InputContext<'a>>;

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
fn many0_in<'a, T: Clone + InputLength, O, E: ParseError<Input<'a, T>>>(
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

fn nfo<'a, I, O, E: ParseError<Input<'a, I>>, F>(
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

fn stmt<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Statement<'a>> {
    branch::alt((
        nfo(directive).map(Into::into),
        nfo(input_bind).map(Into::into),
        nfo(output_bind).map(Into::into),
        nfo(constant_decl).map(Into::into),
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

fn array_index<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Nfo<u32>> {
    tkn("[")
        .ignore_then(nfo(nmc::u32).req("expected index"))
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
    let word = pican_core::alloc::collections::String::from_iter_in(word, i.extra.area);
    Ok((i, word.into_bump_str()))
}
fn register<'a, 'p>(mut i: Input<'a, &'p str>) -> Pres<'a, 'p, Register> {
    for reg_kind in RegisterKind::all() {
        let p = nmc::char(reg_kind.prefix()).ignore_then(nmc::u32.ctx("expected register index"));
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
    ncm::fail.ctx("unknown register prefix").parse(i)
}
fn register_bind<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, RegisterBind<'a>> {
    let (i, _) = tag(".alias")(i)?;
    let (i, _) = space(i)?;
    let (i, name) = nfo(ident.req("expected identifier for alias"))(i)?;
    let (i, _) = space(i)?;
    let swiz = swizzle_expr(register.map(Into::into).or(ident.map(Into::into)))
        .req("expected register for alias");
    let (i, reg) = nfo(swiz)(i)?;
    Ok((i, RegisterBind { name, reg }))
}

fn ident<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Ident<'a>> {
    let not_reg = ncm::not(register);
    let not_op = ncm::not(opcode);
    not_reg
        .ctx("expected identifier but got register name")
        .ignore_then(not_op.ctx("expected identifier but got opcode"))
        .ignore_then(ident_word)
        .map(Ident::new)
        .parse(i)
}
fn f32<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, f32> {
    let frac_part = nom::multi::fold_many1(
        nmc::satisfy(|ch| ch.is_ascii_digit()),
        || (0.0, 10.0),
        |(l, n), c| (l + c.to_digit(10).unwrap() as f32 / n, n + 10.0),
    )
    .map(|(l, _)| l);
    let (i, (negi, ipart, _, floating)) = tuple((
        ncm::opt(tag("-")).map(|v| v.is_some()),
        nmc::u32,
        tag("."),
        frac_part.req("missing floating part"),
    ))
    .parse(i)?;
    Ok((i, ((ipart as f32) + floating) * if negi { -1. } else { 1. }))
}
fn float<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Float> {
    let (i, val) = branch::alt((f32, ncm::map(nmc::i64, |n| n as f32)))
        .ctx("float")
        .parse(i)?;
    if let Some(f) = Float::new(val) {
        Ok((i, f))
    } else {
        ncm::fail.ctx("invalid float, was NaN or inf").parse(i)
    }
}

fn constant_decl<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, ConstantDecl<'a>> {
    fn parse_values<'a, 'p, T: Copy, P, E>(
        mut p: P,
    ) -> impl FnMut(Input<'a, &'p str>) -> Pres<'a, 'p, &'a [T], Input<'a, &'p str>, E>
    where
        P: Parser<Input<'a, &'p str>, T, E>,
        E: nom::error::ParseError<nom_locate::LocatedSpan<&'p str, InputContext<'a>>>
            + nom::error::ContextError<nom_locate::LocatedSpan<&'p str, InputContext<'a>>>,
    {
        move |i| {
            tkn("(")
                .ignore_then(
                    nom::multi::separated_list1(tkn(","), |i| p.parse(i))
                        .req("expected values for constant"),
                )
                .then_ignore(tkn(")").req("missing closing )"))
                .req("missing values for integer constant")
                .map(|ps| i.extra.alloc_slice(&ps))
                .parse(i)
        }
    }
    let (i, _) = tag(".const")(i)?;
    let (i, discrim) = penum(&[
        ("fa", ConstantDiscriminants::FloatArray),
        ("f", ConstantDiscriminants::Float),
        ("i", ConstantDiscriminants::Integer),
    ])
    .req("unknown postfix for constant")
    .parse(i)?;
    let (i, _) = ncm::opt(space)(i)?;
    let (i, name) = nfo(ident)
        .req("expected identifier for constant")
        .parse(i)?;
    let (i, value) = match discrim {
        ConstantDiscriminants::Integer => {
            let (i, values) = nfo(parse_values(nfo(nmc::i32)))(i)?;
            let s = values.span();
            (i, IrNode::new(Constant::Integer(values), s))
        }
        ConstantDiscriminants::Float => {
            let (i, values) = nfo(parse_values(nfo(float)))(i)?;
            let s = values.span();
            (i, IrNode::new(Constant::Float(values), s))
        }
        ConstantDiscriminants::FloatArray => {
            let p = |i| {
                let (i, hint) = nfo(tkn("[")
                    .ignore_then(ncm::opt(nmc::u8))
                    .then_ignore(tkn("]")))
                .req("expected [hint] for constfa (hint is optional)")
                .parse(i)?;
                let (i, values) = nfo(many0_in(nfo(
                    tkn(".constfa").ignore_then(parse_values(nfo(float)))
                )))
                .parse(i)?;
                let (i, _) = tkn(".end").req("missing constfa end").parse(i)?;
                Ok((
                    i,
                    Constant::FloatArray {
                        elements: values.map(|v| v.into_bump_slice()),
                        hint: hint.transpose(),
                    },
                ))
            };
            nfo(p).parse(i)?
        }
    };
    Ok((i, ConstantDecl { name, value }))
}

fn dims<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, u8> {
    tag("[")
        .ignore_then(nmc::u8.req("missing size in uniform dims"))
        .then_ignore(tkn("]").req("missing closing ] for uniform dimensions"))
        .parse(i)
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
        let (i, _) = nmc::multispace0(i)?; // consume whitespace so its part of our span and not the first statement's
        let (i, info) = nfo(many_till(
            nfo(stmt.ctx("program statement").then_ignore(nmc::multispace0)),
            tkn(".end").ctx("block end"),
        )
        .ctx("program statements"))(i)?;
        let statements = info.map(|(val, _)| i.extra.alloc_slice(&val)).map(|v| v);
        Ok((i, Block { statements }))
    }

    let (i, name) = nfo(tag(".proc ").ignore_then(ident.req("missing identifier after '.'")))(i)?;
    let (i, block) = nfo(block).req("missing block end").parse(i)?;
    Ok((i, FunctionDecl { name, block }))
}

fn output_property<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, OutputProperty> {
    penum(&[
        ("position", OutputProperty::Position),
        ("pos", OutputProperty::Position),
        ("normalquat", OutputProperty::NormalQuat),
        ("nquat", OutputProperty::NormalQuat),
        ("color", OutputProperty::Color),
        ("clr", OutputProperty::Color),
        ("texcoord0", OutputProperty::TexCoord0),
        ("tcoord0", OutputProperty::TexCoord0),
        ("texcoord0w", OutputProperty::TexCoord0W),
        ("tcoord0w", OutputProperty::TexCoord0W),
        ("texcoord1", OutputProperty::TexCoord1),
        ("tcoord1", OutputProperty::TexCoord1),
        ("texcoord2", OutputProperty::TexCoord2),
        ("tcoord2", OutputProperty::TexCoord2),
        ("view", OutputProperty::View),
        ("dummy", OutputProperty::Dummy),
    ])
    .parse(i)
}
fn directive<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Directive<'a>> {
    let (i, _) = tag(".")(i)?;
    let (i, d) = tag("nodvle")
        .ctx(".nodvle")
        .map(|_| Directive::NoDvle)
        .or(tag("entry")
            .ctx(".entry")
            .ignore_then(nmc::multispace0)
            .ignore_then(nfo(ident.req("expected proc name for .entry")))
            .map(Directive::Entry))
        .parse(i)?;
    Ok((i, d))
}

fn output_bind<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, OutputBind<'a>> {
    let (i, _) = tag(".out")(i)?;
    let (i, _) = space(i)?;
    let (i, alias) = (tag("-").map(|_| None))
        .or(nfo(ident).map(Some))
        .req("missing alias for .out, expecting - or alias identifier")
        .parse(i)?;
    let (i, _) = space(i)?;
    let (i, property) = nfo(output_property)
        .req("unrecognised property name")
        .parse(i)?;
    let (i, _) = space(i)?;
    let (i, register) = ncm::opt(nfo(register))(i)?;
    Ok((
        i,
        OutputBind {
            alias,
            property,
            register,
        },
    ))
}
fn input_bind<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, InputBind<'a>> {
    let alias_version = ncm::verify(register_bind, |r| {
        r.reg
            .get()
            .target
            .get()
            .as_register()
            .filter(|r| r.kind == RegisterKind::Input)
            .is_some()
    })
    .map(|r| InputBind {
        ident: r.name,
        register: Some(
            r.reg
                .into_inner()
                .target
                .map(|r| r.into_register().unwrap()),
        ),
    });
    let (i, alias_v) = ncm::opt(alias_version)(i)?;
    if let Some(v) = alias_v {
        return Ok((i, v));
    }

    let (i, _) = tag(".in")(i)?;
    let (i, _) = space(i)?;
    let (i, name) = nfo(ident)
        .req("expected identifier for input binding")
        .parse(i)?;
    let (i, register) = ncm::opt(nfo(register))(i)?;
    Ok((
        i,
        InputBind {
            ident: name,
            register,
        },
    ))
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
    ncm::fail.ctx("unknown opcode").parse(i)
}

fn swizzle_dims<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, SwizzleDims<'a>> {
    let (i, _) = tag(".")(i)?;
    let (i, expr) = nfo(many0_in(swizzle_dim)).parse(i)?;
    Ok((i, SwizzleDims(expr.map(|e| e.into_bump_slice()))))
}
fn swizzle_expr<'a, 'p, T, E, P>(
    mut p: P,
) -> impl FnMut(Input<'a, &'p str>) -> Pres<'a, 'p, SwizzleExpr<'a, T>, Input<'a, &'p str>, E>
where
    P: Parser<Input<'a, &'p str>, T, E>,
    E: nom::error::ParseError<nom_locate::LocatedSpan<&'p str, InputContext<'a>>>
        + nom::error::ContextError<nom_locate::LocatedSpan<&'p str, InputContext<'a>>>,
    nom::Err<E>: std::convert::From<
        nom::Err<nom::error::VerboseError<nom_locate::LocatedSpan<&'p str, InputContext<'a>>>>,
    >,
{
    move |i| {
        let (i, target) = nfo(|i| p.parse(i)).parse(i)?;
        let (i, swizzle) = ncm::opt(nfo(swizzle_dims))(i)?;
        Ok((i, SwizzleExpr { target, swizzle }))
    }
}

fn swizzle_dim<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, SwizzleDim> {
    penum(&[
        ("x", SwizzleDim::X),
        ("y", SwizzleDim::Y),
        ("z", SwizzleDim::Z),
        ("w", SwizzleDim::W),
    ])
    .parse(i)
}

fn operand_kind<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, OperandKind<'a>> {
    branch::alt((
        nfo(register.ctx("register operand")).map(OperandKind::Register),
        nfo(ident.ctx("identifier operand")).map(OperandKind::Var),
    ))
    .ctx("operand")
    .parse(i)
}

fn operand<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operand<'a>> {
    let (i, kind) = nfo(operand_kind)(i)?;
    let (i, relative_address) = ncm::opt(array_index.ctx("operand relative address"))(i)?;
    let (i, swizzle) = ncm::opt(nfo(swizzle_dims.ctx("operand swizzle")))(i)?;
    Ok((
        i,
        Operand {
            kind,
            relative_address,
            swizzle,
        },
    ))
}
fn operands<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Operands<'a>> {
    let (i, ops) = nom::multi::separated_list0(tkn(","), nfo(operand))(i)?;
    let ops = i.extra.area.alloc_slice_copy(&ops);
    Ok((i, Operands(ops)))
}
fn op<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Op<'a>> {
    let (i, code) = nfo(opcode.ctx("opcode"))(i)?;
    let (i, _) = nmc::space0(i)?;
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

fn input_span<T>(value: Input<'_, T>) -> Span {
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
        many0_in(
            nmc::multispace0
                .ignore_then(nfo(stmt))
                .then_ignore(nmc::multispace0),
        )
        .map(|stmts| stmts.into_bump_slice()),
    )
}
#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use nom::{character::complete::satisfy, combinator::eof, Parser};
    use pican_core::span::Files;
    use pican_core::{
        alloc::Bump,
        register::{Register, RegisterKind},
    };
    use pican_core::{ir::Float, properties::OutputProperty};

    use crate::ast::{OpCode, Stmt, UniformTy};

    use super::{Input, PError};

    struct TestCtx {
        arena: Bump,
    }
    impl TestCtx {
        fn new() -> Self {
            Self { arena: Bump::new() }
        }

        #[allow(unused)]
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
        let mov = ctx.run_parser("mov mlk, r2", super::op).unwrap();
        assert_eq!(mov.opcode.get(), &OpCode::Mov);
        let operands = mov.operands.get().0;
        assert_eq!(operands.len(), 2);
        assert_eq!(
            operands[0].get().kind.get().try_as_var().unwrap().get(),
            "mlk"
        );
    }
    #[test]
    fn parse_ident_with_num() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser("ml", super::ident).unwrap();
        assert_eq!(res, "ml");
    }

    #[test]
    fn cannot_parse_ident_if_reg() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser("r1", super::ident);
        assert!(res.is_err());
    }

    #[test]
    fn parse_operands() {
        let ctx = TestCtx::new();
        let res = ctx.run_parser("smth, smth2", super::operands).unwrap();
        assert_eq!(res.0.len(), 2);
        assert_eq!(
            res.0[0].get().kind.get().try_as_var().unwrap().get(),
            "smth"
        );
        assert_eq!(
            res.0[1].get().kind.get().try_as_var().unwrap().get(),
            "smth2"
        );
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
            res.0[0].get().kind.get().try_as_register().unwrap().get(),
            &Register::from_str("v0").unwrap(),
        );

        assert_eq!(
            res.0[1].get().kind.get().try_as_register().unwrap().get(),
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
        assert_eq!(
            r.reg.get().target.get().try_as_register().unwrap(),
            &Register::from_str("r0").unwrap()
        );
    }

    #[test]
    fn parse_f32_basic() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("4.2", super::f32).unwrap();
        assert_eq!(r, 4.2);
    }

    #[test]
    fn parse_f32_negative() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("-4.2", super::f32).unwrap();
        assert_eq!(r, -4.2);
    }

    #[test]
    fn parse_f32_negative_only() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("-0.5", super::f32).unwrap();
        assert_eq!(r, -0.5);
    }

    #[test]
    fn parse_f32_negative_tkn() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("-4.2", super::ws(super::f32)).unwrap();
        assert_eq!(r, -4.2);
    }
    #[test]
    fn parse_f32_zero() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("0.0", super::f32).unwrap();
        assert_eq!(r, 0.);
    }

    #[test]
    fn parse_float_zero() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("0.0", super::float).unwrap();
        assert_eq!(r, Float::new(0.0).unwrap());
    }

    #[test]
    fn parse_output_binding() {
        let ctx = TestCtx::new();
        let r = ctx
            .run_parser(".out n tcoord0", super::output_bind)
            .unwrap();
        assert_eq!(r.alias.unwrap().get(), "n");
        assert_eq!(r.property.get(), &OutputProperty::TexCoord0);
    }
    #[test]
    fn parse_property_name() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("pos", super::output_property).unwrap();
        assert_eq!(r, OutputProperty::Position);
    }

    #[test]
    fn parse_end() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser("end", super::op).unwrap();
        assert_eq!(r.opcode.get(), &OpCode::End);
    }
    #[test]
    fn alias_to_input_is_parsed_as_input() {
        let ctx = TestCtx::new();
        let r = ctx.run_parser(".alias m v0", super::stmt).unwrap();
        assert_eq!(
            r.try_as_input_bind()
                .unwrap()
                .into_inner()
                .register
                .unwrap()
                .into_inner(),
            Register::new(RegisterKind::Input, 0)
        );
    }
    #[test]
    fn parse_negative_constant() {
        let ctx = TestCtx::new();
        let r = ctx
            .run_parser(".constf m(0.0, 0.5, 1.0, -0.5)", super::constant_decl)
            .unwrap();
        let vs = r
            .value
            .into_inner()
            .as_float()
            .unwrap()
            .into_inner()
            .iter()
            .map(|f| f.into_inner())
            .collect::<Vec<_>>();
        assert_eq!(
            &vs,
            &[
                Float::new(0.0).unwrap(),
                Float::new(0.5).unwrap(),
                Float::new(1.0).unwrap(),
                Float::new(-0.5).unwrap(),
            ]
        );
    }
}
