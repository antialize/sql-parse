// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use alloc::vec;
use alloc::{boxed::Box, vec::Vec};

use crate::{
    expression::{parse_expression, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    span::OptSpanned,
    statement::parse_compound_query,
    Identifier, Span, Spanned, Statement,
};

#[derive(Debug, Clone)]
pub struct SelectExpr<'a> {
    pub expr: Expression<'a>,
    pub as_: Option<Identifier<'a>>,
}

impl<'a> Spanned for SelectExpr<'a> {
    fn span(&self) -> Span {
        self.expr.join_span(&self.as_)
    }
}

pub(crate) fn parse_select_expr<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<SelectExpr<'a>, ParseError> {
    let expr = parse_expression(parser, false)?;
    let as_ = if parser.skip_keyword(Keyword::AS).is_some() {
        Some(parser.consume_plain_identifier()?)
    } else {
        None
    };
    Ok(SelectExpr { expr, as_ })
}

#[derive(Debug, Clone)]
pub enum JoinSpecification<'a> {
    On(Expression<'a>, Span),
    Using(Vec<Identifier<'a>>, Span),
}

impl<'a> Spanned for JoinSpecification<'a> {
    fn span(&self) -> Span {
        match &self {
            JoinSpecification::On(v, s) => s.join_span(v),
            JoinSpecification::Using(v, s) => s.join_span(v),
        }
    }
}

#[derive(Debug, Clone)]
pub enum JoinType {
    Inner(Span),
    Cross(Span),
    Normal(Span),
    Straight(Span),
    Left(Span),
    Right(Span),
    Natural(Span),
    NaturalInner(Span),
    NaturalLeft(Span),
    NaturalRight(Span),
}
impl Spanned for JoinType {
    fn span(&self) -> Span {
        match &self {
            JoinType::Inner(v) => v.span(),
            JoinType::Cross(v) => v.span(),
            JoinType::Normal(v) => v.span(),
            JoinType::Straight(v) => v.span(),
            JoinType::Left(v) => v.span(),
            JoinType::Right(v) => v.span(),
            JoinType::Natural(v) => v.span(),
            JoinType::NaturalInner(v) => v.span(),
            JoinType::NaturalLeft(v) => v.span(),
            JoinType::NaturalRight(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TableReference<'a> {
    Table {
        identifier: Vec<Identifier<'a>>,
        as_span: Option<Span>,
        as_: Option<Identifier<'a>>,
    },
    Query {
        query: Box<Statement<'a>>,
        as_span: Option<Span>,
        as_: Option<Identifier<'a>>,
        //TODO collist
    },
    Join {
        join: JoinType,
        left: Box<TableReference<'a>>,
        right: Box<TableReference<'a>>,
        specification: Option<JoinSpecification<'a>>,
    },
}

impl<'a> Spanned for TableReference<'a> {
    fn span(&self) -> Span {
        match &self {
            TableReference::Table {
                identifier,
                as_span,
                as_,
            } => identifier
                .opt_join_span(as_span)
                .opt_join_span(as_)
                .expect("span of table"),
            TableReference::Query {
                query,
                as_span,
                as_,
            } => query.join_span(as_span).join_span(as_),
            TableReference::Join {
                join,
                left,
                right,
                specification,
            } => join
                .join_span(left)
                .join_span(right)
                .join_span(specification),
        }
    }
}

pub(crate) fn parse_table_reference_inner<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<TableReference<'a>, ParseError> {
    // TODO [LATERAL] table_subquery [AS] alias [(col_list)]
    // if parser.skip_token(Token::LParen).is_some() {
    //     let a = parse_table_reference(parser)?;
    //     parser.consume_token(Token::RParen)?;
    //     return Ok(a);
    // }

    match &parser.token {
        Token::Ident(_, Keyword::SELECT) | Token::LParen => {
            let query = parse_compound_query(parser)?;
            let as_span = parser.skip_keyword(Keyword::AS);
            let as_ = if as_span.is_some()
                || (matches!(&parser.token, Token::Ident(_, k) if !k.reserved()))
            {
                Some(parser.consume_plain_identifier()?)
            } else {
                None
            };
            Ok(TableReference::Query {
                query: Box::new(query),
                as_span,
                as_,
            })
        }
        Token::Ident(_, _) => {
            let mut identifier = vec![parser.consume_plain_identifier()?];
            loop {
                if parser.skip_token(Token::Period).is_none() {
                    break;
                }
                identifier.push(parser.consume_plain_identifier()?);
            }

            // index_hint_list:
            //     index_hint [, index_hint] ...

            // index_hint: {
            //     USE {INDEX|KEY}
            //       [FOR {JOIN|ORDER BY|GROUP BY}] ([index_list])
            //   | {IGNORE|FORCE} {INDEX|KEY}
            //       [FOR {JOIN|ORDER BY|GROUP BY}] (index_list)
            // }

            // index_list:
            //     index_name [, index_name] .

            // TODO [PARTITION (partition_names)] [[AS] alias]
            let as_span = parser.skip_keyword(Keyword::AS);
            let as_ = if as_span.is_some()
                || (matches!(&parser.token, Token::Ident(_, k) if !k.reserved()))
            {
                Some(parser.consume_plain_identifier()?)
            } else {
                None
            };

            // TODO [index_hint_list]
            Ok(TableReference::Table {
                identifier,
                as_span,
                as_,
            })
        }
        _ => parser.expected_failure("subquery or identifier"),
    }
}

pub(crate) fn parse_table_reference<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<TableReference<'a>, ParseError> {
    let mut ans = parse_table_reference_inner(parser)?;
    loop {
        let join = match parser.token {
            Token::Ident(_, Keyword::INNER) => JoinType::Inner(
                parser
                    .consume_keyword(Keyword::INNER)?
                    .join_span(&parser.consume_keyword(Keyword::JOIN)?),
            ),
            Token::Ident(_, Keyword::CROSS) => JoinType::Cross(
                parser
                    .consume_keyword(Keyword::CROSS)?
                    .join_span(&parser.consume_keyword(Keyword::JOIN)?),
            ),
            Token::Ident(_, Keyword::JOIN) => {
                JoinType::Normal(parser.consume_keyword(Keyword::JOIN)?)
            }
            Token::Ident(_, Keyword::STRAIGHT_JOIN) => {
                JoinType::Straight(parser.consume_keyword(Keyword::STRAIGHT_JOIN)?)
            }
            Token::Ident(_, Keyword::LEFT) => {
                let left = parser.consume_keyword(Keyword::LEFT)?;
                if let Some(outer) = parser.skip_keyword(Keyword::OUTER) {
                    JoinType::Left(
                        left.join_span(&outer)
                            .join_span(&parser.consume_keyword(Keyword::JOIN)?),
                    )
                } else {
                    JoinType::Left(left.join_span(&parser.consume_keyword(Keyword::JOIN)?))
                }
            }
            Token::Ident(_, Keyword::RIGHT) => {
                let right = parser.consume_keyword(Keyword::RIGHT)?;
                if let Some(outer) = parser.skip_keyword(Keyword::OUTER) {
                    JoinType::Right(
                        right
                            .join_span(&outer)
                            .join_span(&parser.consume_keyword(Keyword::JOIN)?),
                    )
                } else {
                    JoinType::Right(right.join_span(&parser.consume_keyword(Keyword::JOIN)?))
                }
            }
            Token::Ident(_, Keyword::NATURAL) => {
                let natural = parser.consume_keyword(Keyword::NATURAL)?;
                match &parser.token {
                    Token::Ident(_, Keyword::INNER) => JoinType::NaturalInner(
                        natural
                            .join_span(&parser.consume_keywords(&[Keyword::INNER, Keyword::JOIN])?),
                    ),
                    Token::Ident(_, Keyword::LEFT) => {
                        let left = parser.consume_keyword(Keyword::LEFT)?;
                        if let Some(outer) = parser.skip_keyword(Keyword::OUTER) {
                            JoinType::NaturalLeft(
                                left.join_span(&outer)
                                    .join_span(&parser.consume_keyword(Keyword::JOIN)?),
                            )
                        } else {
                            JoinType::NaturalLeft(
                                left.join_span(&parser.consume_keyword(Keyword::JOIN)?),
                            )
                        }
                    }
                    Token::Ident(_, Keyword::RIGHT) => {
                        let right = parser.consume_keyword(Keyword::RIGHT)?;
                        if let Some(outer) = parser.skip_keyword(Keyword::OUTER) {
                            JoinType::NaturalRight(
                                right
                                    .join_span(&outer)
                                    .join_span(&parser.consume_keyword(Keyword::JOIN)?),
                            )
                        } else {
                            JoinType::NaturalRight(
                                right.join_span(&parser.consume_keyword(Keyword::JOIN)?),
                            )
                        }
                    }
                    Token::Ident(_, Keyword::JOIN) => JoinType::Natural(
                        natural.join_span(&parser.consume_keyword(Keyword::JOIN)?),
                    ),
                    _ => parser.expected_failure("'INNER', 'LEFT', 'RIGHT' or 'JOIN'")?,
                }
            }
            _ => break,
        };

        let right = parse_table_reference_inner(parser)?;

        let specification = match &parser.token {
            Token::Ident(_, Keyword::ON) => {
                let on = parser.consume_keyword(Keyword::ON)?;
                let expr = parse_expression(parser, false)?;
                Some(JoinSpecification::On(expr, on))
            }
            Token::Ident(_, Keyword::USING) => {
                let using = parser.consume_keyword(Keyword::USING)?;
                let mut join_column_list = Vec::new();
                loop {
                    join_column_list.push(parser.consume_plain_identifier()?);
                    if parser.skip_token(Token::Comma).is_none() {
                        break;
                    }
                }
                Some(JoinSpecification::Using(join_column_list, using))
            }
            _ => None,
        };

        ans = TableReference::Join {
            join,
            left: Box::new(ans),
            right: Box::new(right),
            specification,
        };
    }
    Ok(ans)
}

#[derive(Debug, Clone)]
pub enum SelectFlag {
    All(Span),
    Distinct(Span),
    DistinctRow(Span),
    HighPriority(Span),
    StraightJoin(Span),
    SqlSmallResult(Span),
    SqlBigResult(Span),
    SqlBufferResult(Span),
    SqlNoCache(Span),
    SqlCalcFoundRows(Span),
}

impl<'a> Spanned for SelectFlag {
    fn span(&self) -> Span {
        match &self {
            SelectFlag::All(v) => v.span(),
            SelectFlag::Distinct(v) => v.span(),
            SelectFlag::DistinctRow(v) => v.span(),
            SelectFlag::HighPriority(v) => v.span(),
            SelectFlag::StraightJoin(v) => v.span(),
            SelectFlag::SqlSmallResult(v) => v.span(),
            SelectFlag::SqlBigResult(v) => v.span(),
            SelectFlag::SqlBufferResult(v) => v.span(),
            SelectFlag::SqlNoCache(v) => v.span(),
            SelectFlag::SqlCalcFoundRows(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum OrderFlag {
    Asc(Span),
    Desc(Span),
    None,
}
impl OptSpanned for OrderFlag {
    fn opt_span(&self) -> Option<Span> {
        match &self {
            OrderFlag::Asc(v) => v.opt_span(),
            OrderFlag::Desc(v) => v.opt_span(),
            OrderFlag::None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Select<'a> {
    pub select_span: Span,
    pub flags: Vec<SelectFlag>,
    pub select_exprs: Vec<SelectExpr<'a>>,
    pub from_span: Option<Span>,
    pub table_references: Option<Vec<TableReference<'a>>>,
    pub where_: Option<(Expression<'a>, Span)>,
    pub group_by: Option<(Span, Vec<Expression<'a>>)>,
    pub having_span: Option<Span>,
    pub window_span: Option<Span>,
    pub order_by: Option<(Span, Vec<(Expression<'a>, OrderFlag)>)>,
    pub limit: Option<(Span, Option<Expression<'a>>, Expression<'a>)>,
}
impl<'a> Spanned for Select<'a> {
    fn span(&self) -> Span {
        self.select_span
            .join_span(&self.flags)
            .join_span(&self.select_exprs)
            .join_span(&self.from_span)
            .join_span(&self.table_references)
            .join_span(&self.where_)
            .join_span(&self.group_by)
            .join_span(&self.having_span)
            .join_span(&self.window_span)
            .join_span(&self.order_by)
            .join_span(&self.limit)
    }
}

pub(crate) fn parse_select<'a, 'b>(parser: &mut Parser<'a, 'b>) -> Result<Select<'a>, ParseError> {
    let select_span = parser.consume_keyword(Keyword::SELECT)?;
    let mut flags = Vec::new();
    let mut select_exprs = Vec::new();

    loop {
        match &parser.token {
            Token::Ident(_, Keyword::ALL) => {
                flags.push(SelectFlag::All(parser.consume_keyword(Keyword::ALL)?))
            }
            Token::Ident(_, Keyword::DISTINCT) => flags.push(SelectFlag::Distinct(
                parser.consume_keyword(Keyword::DISTINCT)?,
            )),
            Token::Ident(_, Keyword::DISTINCTROW) => flags.push(SelectFlag::DistinctRow(
                parser.consume_keyword(Keyword::DISTINCTROW)?,
            )),
            Token::Ident(_, Keyword::HIGH_PRIORITY) => flags.push(SelectFlag::HighPriority(
                parser.consume_keyword(Keyword::HIGH_PRIORITY)?,
            )),
            Token::Ident(_, Keyword::STRAIGHT_JOIN) => flags.push(SelectFlag::StraightJoin(
                parser.consume_keyword(Keyword::STRAIGHT_JOIN)?,
            )),
            Token::Ident(_, Keyword::SQL_SMALL_RESULT) => flags.push(SelectFlag::SqlSmallResult(
                parser.consume_keyword(Keyword::SQL_SMALL_RESULT)?,
            )),
            Token::Ident(_, Keyword::SQL_BIG_RESULT) => flags.push(SelectFlag::SqlBigResult(
                parser.consume_keyword(Keyword::SQL_BIG_RESULT)?,
            )),
            Token::Ident(_, Keyword::SQL_BUFFER_RESULT) => flags.push(SelectFlag::SqlBufferResult(
                parser.consume_keyword(Keyword::SQL_BUFFER_RESULT)?,
            )),
            Token::Ident(_, Keyword::SQL_NO_CACHE) => flags.push(SelectFlag::SqlNoCache(
                parser.consume_keyword(Keyword::SQL_NO_CACHE)?,
            )),
            Token::Ident(_, Keyword::SQL_CALC_FOUND_ROWS) => flags.push(
                SelectFlag::SqlCalcFoundRows(parser.consume_keyword(Keyword::SQL_CALC_FOUND_ROWS)?),
            ),
            _ => break,
        }
    }

    loop {
        select_exprs.push(parse_select_expr(parser)?);
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }

    // TODO [into_option]

    let from_span = match parser.skip_keyword(Keyword::FROM) {
        Some(v) => Some(v),
        None => {
            return Ok(Select {
                select_span,
                flags,
                select_exprs,
                from_span: None,
                table_references: None,
                where_: None,
                group_by: None,
                having_span: None,
                window_span: None,
                order_by: None,
                limit: None,
            })
        }
    };

    let mut table_references = Vec::new();
    loop {
        table_references.push(parse_table_reference(parser)?);
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }

    // TODO PARTITION partition_list;
    let where_ = if let Some(span) = parser.skip_keyword(Keyword::WHERE) {
        Some((parse_expression(parser, false)?, span))
    } else {
        None
    };

    let group_by = if let Some(group_span) = parser.skip_keyword(Keyword::GROUP) {
        let span = parser.consume_keyword(Keyword::BY)?.join_span(&group_span);
        let mut groups = Vec::new();
        loop {
            groups.push(parse_expression(parser, false)?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        // TODO [WITH ROLLUP]]
        Some((span, groups))
    } else {
        None
    };

    let having_span = parser.skip_keyword(Keyword::HAVING);
    if having_span.is_some() {
        //TODO where_condition
    }
    let window_span = parser.skip_keyword(Keyword::WINDOW);
    if window_span.is_some() {
        //TODO window_name AS (window_spec) [, window_name AS (window_spec)] ...]
    }

    let order_by = if let Some(span) = parser.skip_keyword(Keyword::ORDER) {
        let span = parser.consume_keyword(Keyword::BY)?.join_span(&span);
        let mut order = Vec::new();
        loop {
            let e = parse_expression(parser, false)?;
            let f = match &parser.token {
                Token::Ident(_, Keyword::ASC) => OrderFlag::Asc(parser.consume()),
                Token::Ident(_, Keyword::DESC) => OrderFlag::Desc(parser.consume()),
                _ => OrderFlag::None,
            };
            order.push((e, f));
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Some((span, order))
    } else {
        None
    };

    let limit = if let Some(span) = parser.skip_keyword(Keyword::LIMIT) {
        let n = parse_expression(parser, true)?;
        match parser.token {
            Token::Comma => {
                parser.consume();
                Some((span, Some(n), parse_expression(parser, true)?))
            }
            Token::Ident(_, Keyword::OFFSET) => {
                parser.consume();
                Some((span, Some(parse_expression(parser, true)?), n))
            }
            _ => Some((span, None, n)),
        }
    } else {
        None
    };
    // TODO [into_option]
    // [FOR {UPDATE | SHARE}
    //     [OF tbl_name [, tbl_name] ...]
    //     [NOWAIT | SKIP LOCKED]
    //   | LOCK IN SHARE MODE]
    // [into_option]

    // into_option: {
    // INTO OUTFILE 'file_name'
    //     [CHARACTER SET charset_name]
    //     export_options
    // | INTO DUMPFILE 'file_name'
    // | INTO var_name [, var_name] ...
    // }

    Ok(Select {
        select_span,
        flags,
        select_exprs,
        from_span,
        table_references: Some(table_references),
        where_,
        group_by,
        having_span,
        window_span,
        order_by,
        limit,
    })
}
