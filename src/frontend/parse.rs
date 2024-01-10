use codespan::{FileId, Location};
use nom::error::{VerboseError, VerboseErrorKind};
use nom_locate::LocatedSpan;

use crate::ir::{IrNode, Span};

#[derive(Clone, Copy)]
struct InputContext<'a> {
    area: &'a bumpalo::Bump,
    file: FileId,
}
impl<'a> InputContext<'a> {
    pub fn alloc<A: Copy>(&self, val: A) -> &'a mut A {
        self.area.alloc(val)
    }
    pub fn span(&self, start: usize, end: usize) -> Span {
        Span::new(start, end, self.file)
    }
}

pub(super) mod comb {
    use std::rc::Rc;

    use codespan::FileId;
    use nom::bytes::complete::take_until;
    use nom::character::complete as nmc;
    use nom::error::context;
    use nom::multi::{many0, many0_count, many1, many_till};
    use nom::sequence::{pair, preceded, separated_pair, terminated, tuple};
    use nom::{
        branch::{self},
        bytes::complete::tag,
        error::ParseError,
        sequence::delimited,
        IResult,
    };
    use nom::{combinator as ncm, FindSubstring, InputIter};
    use nom::{AsChar, Compare, InputLength, InputTake, InputTakeAtPosition, Parser};
    use nom_locate::LocatedSpan;

    use crate::frontend::ast::{Block, Ident, Op, OpCode, Operand, Operands, Stmt};
    use crate::frontend::parse_ext::ParserExt;
    use crate::ir::IrNode;

    use super::InputContext;

    type Nfo<T> = IrNode<T>;

    type Pres<'a, 'p, T, I = Input<'a, &'p str>, E = nom::error::VerboseError<I>> =
        IResult<I, T, E>;
    type NfoRes<'a, 'p, T> = Pres<'a, 'p, Nfo<T>>;
    type Input<'a, T> = LocatedSpan<T, InputContext<'a>>;

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

    fn penum<
        'a,
        'p,
        E: Copy,
        Error: ParseError<nom_locate::LocatedSpan<&'p str, InputContext<'a>>>,
    >(
        tags: &[(&str, E)],
    ) -> impl Parser<Input<'a, &'p str>, E, Error> {
        move |i| {
            for (tag, val) in tags {
                if let Ok((i, _)) = tkn(*tag).parse(i) {
                    return Ok((i, *val));
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
    fn nfo<'a, I, O, E: ParseError<Input<'a, I>>, F>(
        inner: F,
    ) -> impl FnMut(Input<I>) -> IResult<Input<'a, I>, Nfo<O>, E>
    where
        F: Parser<Input<'a, I>, O, E>,
    {
        move |i| {
            let start = i.location_offset() as u32;
            let (i, v) = inner.parse(i)?;
            let ctx = i.extra;
            let end = i.location_offset() as u32;
            let span = ctx.span(start, end);
            Ok((i, Nfo::new(v, span)))
        }
    }

    fn value<I, O: Clone, Oe, E: ParseError<Input<I>>, F>(
        val: O,
        inner: F,
    ) -> impl FnMut(Input<I>) -> IResult<Input<I>, Nfo<O>, E>
    where
        F: Parser<Input<I>, Oe, E>,
    {
        nfo(ncm::value(val, inner))
    }

    fn stmt<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Stmt<'a>> {
        branch::alt((op,)).parse(i)
    }

    pub fn block<'a, 'p>(i: Input<'a, &'p str>) -> NfoRes<'a, 'p, Block<'a>> {
        nfo(ncm::map(
            tuple((many0(stmts), ncm::opt(retr_expr))),
            |(stmts, retr)| {
                Block::new(
                    stmts.into_iter().map(|l| l.lift_rc()).collect(),
                    retr.map(|n| n.map(Rc::new)),
                )
            },
        ))
        .parse(i)
    }
    fn retr_expr(i: Input<&str>) -> NfoRes<RetrExpr> {
        nfo(ncm::map(
            tuple((tkn("return"), ncm::opt(expr_list), ncm::opt(tkn(";")))),
            |(_, exprs, _)| match exprs {
                Some(nfo) => nfo.map(|n| RetrExpr(Some(n))).into_inner(),
                None => RetrExpr(None),
            },
        ))(i)
    }
    fn expr_list(i: Input<&str>) -> NfoRes<ExprList> {
        nfo(ncm::map(
            nom::multi::separated_list1(tkn(","), expr),
            |exprs| ExprList {
                exprs: exprs.into_iter().map(|n| n.map(Rc::new)).collect(),
            },
        ))(i)
    }
    fn if_(i: Input<&str>) -> NfoRes<IfStmt> {
        let else_ = tuple((tkn("else"), ncm::cut(block)));
        let else_ = ncm::map(else_, |(_, b)| b);
        let elseif = tuple((
            tkn("elseif"),
            expr.cut().ctx("missing condition expression"),
            tkn("then"),
            block.cut().ctx("missing block"),
        ));
        let elseif = ncm::map(nfo(elseif), |info| {
            info.map(|(_, cond, _, then)| IfStmt {
                cond,
                then,
                else_: None,
            })
        });
        nfo(delimited(
            tkn("if"),
            ncm::cut(tuple((
                separated_pair(expr, tkn("then"), block),
                many0(elseif),
                ncm::opt(else_),
            ))),
            tkn("end"),
        )
        .map(|((cond, then), elseifs, else_)| {
            let elseifs = elseifs.into_iter().reduce(|prev, mut me| {
                let span = prev.span();
                me.get_mut()
                    .else_
                    .replace(Nfo::new_unique(Block::single(prev.lift_rc()), span));
                me
            });
            let else_ = elseifs
                .map(|mut if_| {
                    let ifspan = if_.span();
                    assert_eq!(if_.get().else_, None);
                    if_.get_mut().else_ = else_.clone();
                    Nfo::new_unique(Block::single(if_.lift_rc()), ifspan)
                })
                .or(else_);
            IfStmt { cond, then, else_ }
        }))(i)
    }
    fn binop_kind(i: Input<&str>) -> Pres<BinOpKind> {
        for (k, op) in BinOpKind::lookup_table() {
            let (i, v) = ncm::opt(tkn(*op))(i)?;
            if v.is_some() {
                return Ok((i, *k));
            }
        }
        ncm::fail(i)
    }
    fn unop_kind(i: Input<&str>) -> Pres<UnOpKind> {
        for (k, op) in UnOpKind::lookup_table() {
            let (i, v) = ncm::opt(tkn(*op))(i)?;
            if v.is_some() {
                return Ok((i, *k));
            }
        }
        ncm::fail(i)
    }

    pub fn expr(i: Input<&str>) -> NfoRes<Expr> {
        let nil = value(Expr::Nil, tkn("nil"));
        let nested = delimited(
            tkn("("),
            expr.req("expected expression"),
            tkn(")").req("missing closing )"),
        );
        let unop = spanned(unop_kind).and(expr).map(|(kind, expr)| UnOpExpr {
            kind,
            on: expr.lift_rc(),
        });
        let prefix = nfo(unop).map(|u| u.lift()).or(nested).or(branch::alt((
            nil,
            value(Expr::TripleDot, tkn("...")),
            lift(bool_expr),
            lift(numeric_lit),
            lift(call_expr),
            lift(table_access),
            nfo(function_expr).map(|f| f.lift()),
            nfo(table_expr).map(|t| t.lift()),
            nfo(string_lit).map(|l| l.lift()),
            ncm::map(nfo(ident), |i| i.lift()),
        )));
        enum Postfix {
            TableAccess {
                kind: Spanned<TableAccessKind>,
            },
            BinOp {
                kind: Spanned<BinOpKind>,
                right: Nfo<Expr>,
            },
        }
        // table access nesting
        let tbl_post = spanned(table_index).map(|kind| Postfix::TableAccess { kind });
        let binop_post = spanned(binop_kind)
            .and(expr.req("missing right of binary operation"))
            .map(|(k, next)| Postfix::BinOp {
                kind: k,
                right: next,
            });

        space
            .ignore_then(prefix)
            .and(ncm::opt(spanned(branch::alt((binop_post, tbl_post)))))
            .map(|(pre, next)| {
                if let Some(next) = next {
                    let span = pre.span().join(next.span());
                    let inner: Expr = match next.into_inner() {
                        Postfix::TableAccess { kind } => {
                            TableAccessExpr::new(kind, pre.lift_rc()).into()
                        }
                        Postfix::BinOp { kind, right } => BinOpExpr {
                            left: pre.lift_rc(),
                            kind,
                            right: right.lift_rc(),
                        }
                        .into(),
                    };
                    Nfo::new_unique(inner, span)
                } else {
                    pre
                }
            })
            .then_ignore(space)
            .parse(i)
    }
    fn call_expr(i: Input<&str>) -> NfoRes<CallExpr> {
        let (i, _) = space(i)?;
        let (i, prefix): (_, Nfo<Expr>) = branch::alt((
            table_access.map(|t| t.lift()),
            ncm::map(nfo(ident), |i| i.lift()),
        ))(i)?;
        let call = |i| {
            let args_parens = delimited(
                tkn("("),
                nom::multi::separated_list0(tkn(","), expr.map(|e| e.lift_rc())),
                tkn(")").req("missing closing )"),
            );
            let args_tbl = ncm::map(nfo(table_expr), |tbl| -> Vec<Nrc<Expr>> {
                vec![tbl.lift_rc()]
            });
            let args_str = ncm::map(nfo(string_lit), |lit| vec![lit.lift_rc()]);
            let args = branch::alt((args_parens, args_tbl, args_str));
            spanned(
                ncm::opt(preceded(
                    tkn(":"),
                    spanned(ident).req("missing identifier for table access"),
                ))
                .and(args),
            )(i)
        };
        let (i, calls) = many1(call)(i)?;
        let call = calls.into_iter().fold(
            prefix,
            |prefix,
             Spanned {
                 value: (method_access, args),
                 span,
             }| {
                let prefix = prefix.lift_rc();
                let r = if let Some(ident) = method_access {
                    CallExpr::new(
                        Nfo::new_unique(
                            TableAccessExpr::new(
                                Spanned::new(ident.span(), TableAccessKind::Field(ident)),
                                prefix.clone(),
                            ),
                            span,
                        )
                        .lift_rc(),
                        std::iter::once(prefix).chain(args.into_iter()).collect(),
                    )
                } else {
                    CallExpr::new(prefix, args.into())
                };
                Nfo::new_unique(r.into(), span)
            },
        );

        Ok((i, call.map(|c| c.into_call().unwrap())))
    }
    fn func_body(i: Input<&str>) -> Pres<FuncBody> {
        let param = ident
            .map(FuncParam::Ident)
            .or(ncm::value(FuncParam::Variadic, tkn("...")));
        let (i, params) = delimited(
            tkn("("),
            ncm::into(nom::multi::separated_list0(tkn(","), spanned(param))),
            tkn(")").req("missing closing )"),
        )(i)?;
        let (i, body) = terminated(spanned(block.map(|b| b.into_inner())), tkn("end"))(i)?;
        let body = FuncBody { params, body };
        Ok((i, body))
    }
    fn function_stmt(i: Input<&str>) -> Pres<FunctionStmt> {
        let tbl_access_name =
            tkn(".").ignore_then(nom::multi::separated_list1(tkn("."), spanned(ident)));

        let (i, local) = ncm::opt(tkn("local"))(i)?;
        let local = local.is_some();
        let (i, _) = tkn("function")(i)?;
        let (i, mut name) = spanned(ident)
            .and(ncm::cond(!local, ncm::opt(spanned(tbl_access_name))).map(|f| f.flatten()))
            .map(|(prefix, tbl_access)| {
                if let Some(fields) = tbl_access {
                    let span = prefix.span().join(fields.span());
                    Spanned::new(
                        span,
                        FunctionNameTableAccess {
                            table: prefix,
                            path: fields.into_inner().into(),
                        }
                        .into(),
                    )
                } else {
                    prefix.lift()
                }
            })
            .req("missing function name")
            .parse(i)?;
        let (i, access) = ncm::cond(!local, ncm::opt(tkn(":").ignore_then(spanned(ident))))
            .map(|f| f.flatten())
            .parse(i)?;
        let (i, mut body) = func_body(i)?;
        if let Some(access) = access {
            body.params = std::iter::once(access.clone().map(|_| Ident::new("self").into()))
                .chain(body.params.iter().cloned())
                .collect();
            let nspan = name.span();
            name = name.map(|n| match n {
                FunctionName::Ident(i) => FunctionNameTableAccess {
                    table: Spanned::new(nspan, i),
                    path: Rc::new([access]),
                }
                .into(),
                FunctionName::TableAccess(t) => FunctionNameTableAccess {
                    table: t.table,
                    path: t
                        .path
                        .iter()
                        .cloned()
                        .chain(std::iter::once(access))
                        .collect(),
                }
                .into(),
            });
        }
        Ok((i, FunctionStmt { name, local, body }))
    }
    fn function_expr(i: Input<&str>) -> Pres<FunctionExpr> {
        tkn("function")
            .ignore_then(func_body)
            .map(|b| FunctionExpr { body: b })
            .parse(i)
    }
    fn table_expr(i: Input<&str>) -> Pres<TableExpr> {
        let p = |i| {
            let ident_key = ncm::into(spanned(ident));
            let escaped_key = delimited(
                tkn("["),
                ncm::into(expr).req("expected expression"),
                tkn("]").req("missing closing ]"),
            );
            let tbl_key = branch::alt((escaped_key, ident_key));
            let map_field = ncm::map(
                separated_pair(
                    tbl_key,
                    tkn("="),
                    expr.cut().ctx("missing operand to assign"),
                ),
                |(l, r)| TableField::Map { key: l, value: r },
            );
            let list_field = ncm::into(expr);
            let field_sep = || branch::alt((tkn(","), tkn(";")));
            let (i, fields): (_, Vec<TableField>) = delimited(
                tkn("{"),
                nom::multi::separated_list0(
                    field_sep(),
                    branch::alt((ncm::into(map_field), list_field)),
                ),
                pair(ncm::opt(field_sep()), tkn("}").req("missing closing }")),
            )(i)?;
            let tbl = TableExpr {
                fields: fields.into(),
            };
            Ok((i, tbl))
        };
        context("table", p)(i)
    }
    fn table_index(i: Input<&str>) -> Pres<TableAccessKind> {
        let index = ncm::map(
            tuple((
                tkn("["),
                expr.req("expected expression"),
                tkn("]").req("missing closing ]"),
            )),
            |(_, e, _)| TableAccessKind::Index(e.lift_rc()),
        );
        let field = tkn(".")
            .ignore_then(ncm::not(tkn(".") /* .. might be a concat */))
            .ignore_then(spanned(ident.req("expected identifier")))
            .map(TableAccessKind::Field);
        index.or(field).parse(i)
    }
    fn table_access(i: Input<&str>) -> NfoRes<TableAccessExpr> {
        let (i, (init, accesses)) = tuple((nfo(ident), many1(nfo(table_index))))(i)?;
        let expr: Nrc<Expr> = accesses.into_iter().fold(init.lift_rc(), |on, i| {
            let extra_span = i.span();
            let full_span = on.span().join(extra_span);
            Nfo::new_unique(
                Rc::new(TableAccessExpr::new(i.into_spanned(), on).into()),
                full_span,
            )
        });
        let tbl = expr.map(|expr| Rc::try_unwrap(expr).unwrap().into_table_access().unwrap());
        Ok((i, tbl))
    }
    fn table_assign(i: Input<&str>) -> Pres<TableAssignStmt> {
        let p = separated_pair(
            table_access,
            tkn("="),
            expr.req("missing operand to assign").map(|e| e.lift_rc()),
        );
        p.map(|(target, to)| TableAssignStmt { target, to })
            .parse(i)
    }
    fn ident_char(i: Input<&str>) -> Pres<char> {
        branch::alt((
            nmc::satisfy(|ch| ch.is_alpha()),
            ncm::value('_', nmc::char('_')),
        ))
        .parse(i)
    }
    fn ident_word<'a>(i: Input<'a, &str>) -> Pres<&'a str> {
        let ident_word = tuple((
            ident_char,
            many0(branch::alt((
                ident_char,
                nmc::satisfy(|ch| ch.is_dec_digit()),
            ))),
        ));
        ncm::map(ident_word, |(pre, rest)| {
            let s = format!("{pre}{rest}", rest = rest.into_iter().collect::<String>());
            i.area.alloc_str(&s)
        })(i)
    }
    fn ident<'a, 'p>(i: Input<'a, &'p str>) -> Pres<'a, 'p, Ident<'a>> {
        let keywords = or_kw! {
            mov, dp4
        };

        ncm::map(
            preceded(
                ncm::not(keywords.and(ncm::not(
                    nmc::alpha1.map(|_| ()).or(nmc::char('_').map(|_| ())),
                ))),
                ident_word,
            ),
            Ident::new,
        )(i)
    }
    fn f64(i: Input<&str>) -> Pres<f64> {
        let (i, (ipart, _, floating)) =
            tuple((nmc::i64, tag("."), nmc::u64.req("missing floating part"))).parse(i)?;
        Ok((i, (ipart as f64) + 1.0f64 / (floating as f64)))
    }
    fn string_lit(i: Input<&str>) -> Pres<StringLit> {
        let long = |i| {
            // lua strings are somewhat insane, this bit is for [=[]=], the equals have to be the same length or it isn't the end of the string
            let (i, eqs) = delimited(
                tkn("["),
                many0_count(tkn("=")),
                tuple((tkn("["), ncm::opt(nmc::char('\n')))),
            )(i)?;
            let close = delimited(tkn("]"), many0_count(tkn("=")), tkn("]"));
            let closeeq = ncm::verify(close, |close_eqs| *close_eqs == eqs);
            let (i, (content, _)) = many_till(nmc::anychar, closeeq)(i)?;
            Ok((i, content))
        };
        let escaped = || preceded(nmc::char('\\'), nmc::anychar);
        let double = delimited(
            space.ignore_then(tag("\"")),
            many0(branch::alt((
                escaped(),
                preceded(ncm::not(nmc::one_of("\"\n")), nmc::anychar),
            ))),
            tag("\"")
                .then_ignore(space)
                .req("missing string closing \""),
        );
        let single = delimited(
            space.ignore_then(tag("'")),
            many0(branch::alt((
                escaped(),
                preceded(ncm::not(nmc::one_of("'\n")), nmc::anychar),
            ))),
            tag("'").then_ignore(space).req("missing string closing '"),
        );
        let (i, content) = branch::alt((double, single, long))(i)?;
        Ok((i, StringLit::new(content.into_iter().collect::<String>())))
    }
    fn numeric_lit(i: Input<&str>) -> NfoRes<NumericExpr> {
        nfo(branch::alt((
            ncm::map(f64, Into::into),
            ncm::map(nmc::i64, Into::into),
        )))(i)
    }
    fn bool_expr(i: Input<&str>) -> NfoRes<BoolExpr> {
        branch::alt((
            value(BoolExpr::Lit(true), tkn("true")),
            value(BoolExpr::Lit(false), tkn("false")),
        ))
        .ctx("boolean expression")
        .parse(i)
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
        let (i, code) = nfo(opcode)(i)?;
        let (i, operands) = nfo(operands)(i)?;
        Ok((
            i,
            Op {
                opcode: code,
                operands,
            },
        ))
    }
}
pub type ErrorKind = nom::error::VerboseError<Span>;
impl<T> From<LocatedSpan<T, FileId>> for Span {
    fn from(value: LocatedSpan<T, FileId>) -> Self {
        Self::new(
            value.location_offset() as u32,
            value.location_offset() as u32,
            value.extra,
        )
    }
}

pub fn parse(input: &str, file: FileId) -> Result<Nfo<Block>, ErrorKind> {
    match comb::block(LocatedSpan::new_extra(input, file)) {
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
#[cfg(test)]
mod tests {

    use crate::{
        frontend::ast::{
            Block, CallExpr, Expr, ForLoopKind, Nfo, Nrc, Statement, StringLit, TableField,
            UnOpKind,
        },
        types::{FilesDb, Ident},
    };
    fn extract_retr(b: &Nfo<Block>) -> &'_ Nrc<Expr> {
        &b.get()
            .retr
            .as_ref()
            .unwrap()
            .get()
            .0
            .as_ref()
            .unwrap()
            .exprs[0]
    }
    fn parse(input: &str) -> Result<Nfo<Block>, super::ErrorKind> {
        let mut db = FilesDb::new();
        let id = db.add("test", input.into());
        super::parse(input, id)
    }
    fn parse_fst(input: &str) -> Result<Nrc<Statement>, super::ErrorKind> {
        let st = parse(input)?;
        let fnode = &st.get().body;
        assert_eq!(fnode.len(), 1);
        Ok(fnode[0].to_owned())
    }
    macro_rules! parse_fst_as {
        ( $as:ident, $exp:expr) => {{
            let v = parse_fst($exp).unwrap();
            paste::paste! { (*v.into_inner()).clone(). [<try_into_ $as>]().unwrap() }
        }};
    }
    #[track_caller]
    fn parse_expr(input: &str) -> Nrc<Expr> {
        let v = parse_fst_as!(binding, &format!("local x = {input}"));
        assert_eq!(v.to.len(), 1);
        v.to[0].to_owned()
    }

    #[test]
    fn can_parse_if_without_elseifs() {
        let stmt = parse_fst_as!(if, r"if x then return y else return z end");
        assert_eq!(stmt.cond.get().try_as_ident().unwrap(), &Ident::new("x"));
        assert_eq!(
            extract_retr(&stmt.then).get().try_as_ident().unwrap(),
            &Ident::new("y")
        );
        assert_eq!(
            extract_retr(&stmt.else_.unwrap())
                .get()
                .try_as_ident()
                .unwrap(),
            &Ident::new("z")
        );
    }

    #[test]
    fn can_parse_if_with_elseifs() {
        let stmt = parse_fst_as!(
            if,
            r"if x then return y elseif k then return i else return z end"
        );
        assert_eq!(stmt.cond.get().try_as_ident().unwrap(), &Ident::new("x"));
        let selse = stmt.else_.unwrap();
        let elseif = selse.get().body[0].get().try_as_if().unwrap();
        let elseif_c = elseif.cond.get();
        assert_eq!(elseif_c.try_as_ident().unwrap(), &Ident::new("k"));
        assert_eq!(
            extract_retr(&elseif.then).get().try_as_ident().unwrap(),
            &Ident::new("i")
        );
        assert_eq!(
            extract_retr(elseif.else_.as_ref().unwrap())
                .get()
                .try_as_ident()
                .unwrap(),
            &Ident::new("z")
        );
    }

    #[test]
    fn can_parse_single_var() {
        let st = parse_fst_as!(binding, "local x");
        assert_eq!(st.names.as_ref()[0].get(), &Ident::new("x"));
    }
    #[test]
    fn can_parse_function_call_normal() {
        let r = parse_fst_as!(call, "f(x)");
        assert_eq!(r.target.get().try_as_ident().unwrap(), &Ident::new("f"));
        assert_eq!(r.args.len(), 1);
        assert_eq!(r.args[0].get().try_as_ident().unwrap(), &Ident::new("x"));
    }

    #[test]
    fn can_parse_function_call_str() {
        let r = parse_fst_as!(call, r#"f "y" "#);
        assert_eq!(r.target.get().try_as_ident().unwrap(), &Ident::new("f"));
        assert_eq!(r.args.len(), 1);
        assert_eq!(
            r.args[0].get().try_as_str_lit().unwrap(),
            &StringLit::new("y")
        );
    }
    fn check_parse_function_call_tbl(r: &CallExpr) {
        assert_eq!(r.target.get().try_as_ident().unwrap(), &Ident::new("f"));
        assert_eq!(r.args.len(), 1);
        let tbl = r.args[0].get().try_as_table().unwrap();
        let fields = &tbl.fields;
        assert_eq!(fields.len(), 1);
        let TableField::Map { key: k, value: v } = &fields[0] else {
            panic!("expected map");
        };
        assert_eq!(k.try_as_ident().unwrap().get(), &Ident::new("y"));
        assert_eq!(*v.get().as_num_lit().unwrap().try_as_int().unwrap(), 4);
    }

    #[test]
    fn can_parse_function_call_table() {
        let r = parse_fst_as!(call, r#"f { y = 4 } "#);
        check_parse_function_call_tbl(&r);
    }

    #[test]
    fn function_stmt_table() {
        let _ = parse_fst_as!(function, r#"function a.b() end"#);
        let r = parse_fst_as!(function, r#"function a:b() end"#);
        assert!(r.name.get().is_table_access());
        assert_eq!(
            r.name.get().as_table_access().unwrap().path[0].get(),
            &Ident::new("b")
        );
    }

    #[test]
    fn can_parse_double_function_call_table() {
        let r = parse_fst_as!(call, r#"f { y = 4 } { k }"#);
        check_parse_function_call_tbl(r.target.get().as_call().unwrap());
        assert_eq!(r.args.len(), 1);
        assert_eq!(
            r.args[0].get().try_as_table().unwrap().fields[0]
                .try_as_list()
                .unwrap()
                .get()
                .try_as_ident()
                .unwrap(),
            &Ident::new("k")
        );
    }

    #[test]
    fn can_parse_triple_function_call_table() {
        let _ = parse_fst_as!(call, r#"f { y = 4 } { k } { l }"#);
        let _ = parse_fst_as!(call, r#"f { y = 4 } { k } { l } "c""#);
        let _ = parse_fst_as!(call, r#"f { y = 4 } { ["k"] = c }"#);
    }

    #[test]
    fn parses_binding_multiple() {
        let r = parse("local x, y = 4, 5").unwrap();
        let stmts = &r.get().body;
        assert_eq!(stmts.len(), 1);
        let stmt = stmts[0].get().try_as_binding().unwrap();
        let x = stmt.names[0].get();
        let y = stmt.names[1].get();
        assert_eq!(x, &Ident::new("x"));
        assert_eq!(y, &Ident::new("y"));
        assert_eq!(stmt.to[0].get().try_as_i64().unwrap(), 4);
        assert_eq!(stmt.to[1].get().try_as_i64().unwrap(), 5);
    }
    #[test]
    fn parses_table_access() {
        let r = parse_fst_as!(binding, "local x = k.y");
        let tgt = &r.to[0];
        let access = tgt.get().as_table_access().unwrap();
        assert_eq!(access.on.get().try_as_ident().unwrap(), &Ident::new("k"));
        assert_eq!(
            access.kind.get().try_as_field().unwrap().get(),
            &Ident::new("y")
        );
    }

    #[test]
    fn parses_range_based_for_loop() {
        let r = parse_fst_as!(for_loop, "for x in pairs(y) do end");
        let ForLoopKind::Range { names, exprs } = r.kind else {
            panic!("invalid kind: {:#?}", r.kind);
        };
        assert_eq!(names.len(), 1);
        assert_eq!(exprs.len(), 1);
        assert_eq!(names[0].get(), &Ident::new("x"));
        assert!(exprs[0].get().is_call());
    }

    #[test]
    fn parses_var_based_for_loop() {
        let r = parse_fst_as!(for_loop, "for x = 0, 10 do end");
        let ForLoopKind::Var {
            var,
            initial,
            term_cond,
            update: None,
        } = r.kind
        else {
            panic!("invalid kind: {:#?}", r.kind);
        };
        assert_eq!(var.get(), &Ident::new("x"));
        assert_eq!(*initial.get().as_num_lit().unwrap().as_int().unwrap(), 0);
        assert_eq!(*term_cond.get().as_num_lit().unwrap().as_int().unwrap(), 10);
    }
    #[test]
    fn parses_function_decl() {
        let r = parse_fst_as!(function, "function f() end");
        assert_eq!(r.name.get().try_as_ident().unwrap(), &Ident::new("f"));
        assert!(!r.local);
    }

    #[test]
    fn parses_local_function_decl() {
        let r = parse_fst_as!(function, "local function f() end");
        assert_eq!(r.name.get().try_as_ident().unwrap(), &Ident::new("f"));
        assert!(r.local);
    }
    #[test]
    fn double_local() {
        let r = parse("local x = 2, 3\nlocal y = i").unwrap();
        assert_eq!(r.get().body.len(), 2);
        assert!(r.get().body[0].get().is_binding());
        assert!(r.get().body[1].get().is_binding());
    }
    #[test]
    fn assignments_can_contain_dots() {
        let r = parse_fst_as!(table_assign, "x.y = 2");
        assert!(r.target.get().kind.get().is_field());
    }

    #[test]
    fn strings_can_be_single_quoted() {
        let r = parse_fst_as!(binding, "local x = 'a'");
        assert!(r.local);
        assert_eq!(r.to.len(), 1);
        assert_eq!(r.to[0].get().as_str_lit().unwrap().0.as_ref(), "a");
    }

    #[test]
    fn tables_can_have_a_trailing_comma() {
        let r = parse_fst_as!(binding, "local x = {a = 1, }");
        assert!(r.local);
        assert!(r.to[0].get().is_table());
    }

    #[test]
    fn expression_table_access() {
        let r = parse_fst_as!(binding, "local m = f(m).c");
        assert_eq!(r.to.len(), 1);
        let to = r.to[0].get().try_as_table_access().unwrap();
        assert!(to.on.get().is_call());
        assert_eq!(
            to.kind.get().try_as_field().unwrap().get(),
            &Ident::new("c")
        );
    }

    #[test]
    fn table_function_call() {
        let _ = parse_fst_as!(call, "a.c()");
    }

    #[test]
    fn string_concat() {
        let b = parse_fst_as!(binding, "local a = c .. 'c'");
        assert!(b.to[0].get().is_bin_op());
    }
    #[test]
    fn variables_can_contain_keywords() {
        let _ = parse_fst_as!(call, "a.local_()");
    }

    #[test]
    fn can_assign_functions_to_vars() {
        let b = parse_fst_as!(binding, "local c = function() end");
        assert!(b.to[0].get().is_function());
    }

    #[test]
    fn can_put_brackets_around_expressions() {
        let b = parse_fst_as!(binding, "local c = ('a')");
        assert!(b.to[0].get().is_str_lit());
    }
    #[test]
    fn multi_arg_call() {
        let c = parse_fst_as!(call, "f(a, 'c')");
        assert_eq!(c.args.len(), 2);
        assert_eq!(c.args[1].get().try_as_str_lit().unwrap().0.as_ref(), "c");
    }

    #[test]
    fn nested_string_fncall() {
        let c = parse_fst_as!(binding, "local v = P(a, '--[[c', 'c')\n");
        let c = &c.to[0].get().try_as_call().unwrap().args[1];
        assert_eq!(c.get().try_as_str_lit().unwrap().0.as_ref(), "--[[c");
    }

    #[test]
    fn nested_string() {
        let c = parse_fst_as!(binding, "local v = '--[[c'\n");
        assert_eq!(c.to[0].get().try_as_str_lit().unwrap().0.as_ref(), "--[[c");
    }
    #[test]
    fn unary_op_negate() {
        let c = parse_expr("-6");
        assert_eq!(
            c.get().try_as_un_op().unwrap().kind.get(),
            &UnOpKind::Negate
        );
    }

    #[test]
    fn block_call() {
        let c = parse("local x = z\nf()").unwrap();
        assert_eq!(c.get().body.len(), 2);
    }
    #[test]
    fn block_with_or() {
        let c = parse("local x = z\ntable.c()").unwrap();
        assert_eq!(c.get().body.len(), 2);
    }

    #[test]
    fn triple_dot_is_an_expression() {
        let c = parse_expr("...");
        assert!(c.get().is_triple_dot());
    }

    #[test]
    fn triple_dot_is_a_valid_function_arg() {
        let c = parse_expr("function(...) end");
        assert!(c.get().try_as_function().unwrap().body.params[0]
            .get()
            .is_variadic());
    }
    #[test]
    fn index_access_from_function() {
        let _ = parse_expr("f(x)[c]");
    }
}
