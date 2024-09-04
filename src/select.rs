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
use alloc::{boxed::Box, vec::Vec};

use crate::qualified_name::parse_qualified_name;
use crate::{
    expression::{parse_expression, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    span::OptSpanned,
    statement::parse_compound_query,
    Identifier, Span, Spanned, Statement,
};
use crate::{Issue, QualifiedName};

/// Value in select
#[derive(Debug, Clone)]
pub struct SelectExpr<'a> {
    /// Value to select
    pub expr: Expression<'a>,
    /// Optional name to give value if specified
    pub as_: Option<Identifier<'a>>,
}

impl<'a> Spanned for SelectExpr<'a> {
    fn span(&self) -> Span {
        self.expr.join_span(&self.as_)
    }
}

pub(crate) fn parse_select_expr<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<SelectExpr<'a>, ParseError> {
    let expr = parse_expression(parser, false)?;
    let as_ = if parser.skip_keyword(Keyword::AS).is_some() {
        Some(parser.consume_plain_identifier()?)
    } else {
        None
    };
    Ok(SelectExpr { expr, as_ })
}

/// Specification for join
#[derive(Debug, Clone)]
pub enum JoinSpecification<'a> {
    /// On specification expression and span of "ON"
    On(Expression<'a>, Span),
    /// List of columns to joint using, and span of "USING"
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

/// Type of join
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
pub enum IndexHintUse {
    Use(Span),
    Ignore(Span),
    Force(Span),
}
impl Spanned for IndexHintUse {
    fn span(&self) -> Span {
        match &self {
            IndexHintUse::Use(v) => v.span(),
            IndexHintUse::Ignore(v) => v.span(),
            IndexHintUse::Force(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IndexHintType {
    Index(Span),
    Key(Span),
}
impl Spanned for IndexHintType {
    fn span(&self) -> Span {
        match &self {
            IndexHintType::Index(v) => v.span(),
            IndexHintType::Key(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IndexHintFor {
    Join(Span),
    OrderBy(Span),
    GroupBy(Span),
}
impl Spanned for IndexHintFor {
    fn span(&self) -> Span {
        match &self {
            IndexHintFor::Join(v) => v.span(),
            IndexHintFor::OrderBy(v) => v.span(),
            IndexHintFor::GroupBy(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IndexHint<'a> {
    pub use_: IndexHintUse,
    pub type_: IndexHintType,
    pub for_: Option<(Span, IndexHintFor)>,
    pub lparen: Span,
    pub index_list: Vec<Identifier<'a>>,
    pub rparen: Span,
}

impl<'a> Spanned for IndexHint<'a> {
    fn span(&self) -> Span {
        self.use_
            .span()
            .join_span(&self.type_)
            .join_span(&self.for_)
            .join_span(&self.lparen)
            .join_span(&self.index_list)
            .join_span(&self.rparen)
    }
}

/// Reference to table in select
#[derive(Debug, Clone)]
pub enum TableReference<'a> {
    /// Reference to a table or view
    Table {
        /// Name of table to to select from
        identifier: QualifiedName<'a>,
        /// Span of "AS" if specified
        as_span: Option<Span>,
        /// Alias for table if specified
        as_: Option<Identifier<'a>>,
        /// Index hints
        index_hints: Vec<IndexHint<'a>>,
    },
    /// Subquery
    Query {
        /// Query yielding table
        query: Box<Statement<'a>>,
        /// Span of "AS" if specified
        as_span: Option<Span>,
        /// Alias for table if specified
        as_: Option<Identifier<'a>>,
        //TODO collist
    },
    /// Join
    Join {
        /// What type of join is it
        join: JoinType,
        /// Left hand side of join
        left: Box<TableReference<'a>>,
        /// Right hand side of join
        right: Box<TableReference<'a>>,
        /// How to do the join if specified
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
                index_hints,
            } => identifier
                .opt_join_span(as_span)
                .opt_join_span(as_)
                .opt_join_span(index_hints)
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

pub(crate) fn parse_table_reference_inner<'a>(
    parser: &mut Parser<'a, '_>,
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
            let identifier = parse_qualified_name(parser)?;

            // TODO [PARTITION (partition_names)] [[AS] alias]
            let as_span = parser.skip_keyword(Keyword::AS);
            let as_ = if as_span.is_some()
                || (matches!(&parser.token, Token::Ident(_, k) if !k.reserved()))
            {
                Some(parser.consume_plain_identifier()?)
            } else {
                None
            };

            let mut index_hints = Vec::new();
            loop {
                let use_ = match parser.token {
                    Token::Ident(_, Keyword::USE) => IndexHintUse::Use(parser.consume()),
                    Token::Ident(_, Keyword::IGNORE) => IndexHintUse::Ignore(parser.consume()),
                    Token::Ident(_, Keyword::FORCE) => IndexHintUse::Force(parser.consume()),
                    _ => break,
                };
                let type_ = match parser.token {
                    Token::Ident(_, Keyword::INDEX) => IndexHintType::Index(parser.consume()),
                    Token::Ident(_, Keyword::KEY) => IndexHintType::Key(parser.consume()),
                    _ => parser.expected_failure("'INDEX' or 'KEY'")?,
                };
                let for_ = if let Some(for_span) = parser.skip_keyword(Keyword::FOR) {
                    let v = match parser.token {
                        Token::Ident(_, Keyword::JOIN) => IndexHintFor::Join(parser.consume()),
                        Token::Ident(_, Keyword::GROUP) => IndexHintFor::GroupBy(
                            parser.consume_keywords(&[Keyword::GROUP, Keyword::BY])?,
                        ),
                        Token::Ident(_, Keyword::ORDER) => IndexHintFor::OrderBy(
                            parser.consume_keywords(&[Keyword::ORDER, Keyword::BY])?,
                        ),
                        _ => parser.expected_failure("'JOIN', 'GROUP BY' or 'ORDER BY'")?,
                    };
                    Some((for_span, v))
                } else {
                    None
                };
                let lparen = parser.consume_token(Token::LParen)?;
                let mut index_list = Vec::new();
                loop {
                    parser.recovered(
                        "')' or ','",
                        &|t| matches!(t, Token::RParen | Token::Comma),
                        |parser| {
                            index_list.push(parser.consume_plain_identifier()?);
                            Ok(())
                        },
                    )?;
                    if matches!(parser.token, Token::RParen) {
                        break;
                    }
                    parser.consume_token(Token::Comma)?;
                }
                let rparen = parser.consume_token(Token::RParen)?;
                index_hints.push(IndexHint {
                    use_,
                    type_,
                    for_,
                    lparen,
                    index_list,
                    rparen,
                })
            }

            if !index_hints.is_empty() {
                if !parser.options.dialect.is_maria() {
                    parser.add_error(
                        "Index hints only supported by MariaDb",
                        &index_hints.opt_span().unwrap(),
                    );
                }
            }

            Ok(TableReference::Table {
                identifier,
                as_span,
                as_,
                index_hints,
            })
        }
        _ => parser.expected_failure("subquery or identifier"),
    }
}

pub(crate) fn parse_table_reference<'a>(
    parser: &mut Parser<'a, '_>,
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

/// Flags specified after SELECT
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

impl Spanned for SelectFlag {
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

/// Ordering direction
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

/// Lock strength for locking
#[derive(Debug, Clone)]
pub enum LockStrength {
    Update(Span),
    Share(Span),
    NoKeyUpdate(Span),
    KeyShare(Span),
}
impl Spanned for LockStrength {
    fn span(&self) -> Span {
        match &self {
            LockStrength::Update(v) => v.span(),
            LockStrength::Share(v) => v.span(),
            LockStrength::NoKeyUpdate(v) => v.span(),
            LockStrength::KeyShare(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LockWait {
    NoWait(Span),
    SkipLocket(Span),
    Default,
}
impl OptSpanned for LockWait {
    fn opt_span(&self) -> Option<Span> {
        match &self {
            LockWait::NoWait(v) => v.opt_span(),
            LockWait::SkipLocket(v) => v.opt_span(),
            LockWait::Default => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Locking<'a> {
    /// Span of "FOR"
    pub for_span: Span,
    pub strength: LockStrength,
    pub of: Option<(Span, Vec<Identifier<'a>>)>,
    pub wait: LockWait,
}
impl<'a> Spanned for Locking<'a> {
    fn span(&self) -> Span {
        self.for_span
            .join_span(&self.strength)
            .join_span(&self.of)
            .join_span(&self.wait)
    }
}

/// Representation of select Statement
///
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statement, Select, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "SELECT f1,f2 FROM t1 WHERE f3<=10 AND f4='y'";
/// let stmt = parse_statement(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let s: Select = match stmt {
///     Some(Statement::Select(s)) => s,
///     _ => panic!("We should get an select statement")
/// };
///
/// println!("{:#?}", s.where_);
///
/// let sql = "SELECT CAST(NULL AS CHAR)";
/// let stmt = parse_statement(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let s: Select = match stmt {
///     Some(Statement::Select(s)) => s,
///     _ => panic!("We should get an select statement")
/// };
///
/// println!("{:#?}", s.where_);
///
/// let sql = "SELECT * FROM t1, d2.t2 FOR SHARE OF t1, t2 NOWAIT";
/// let stmt = parse_statement(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let s: Select = match stmt {
///     Some(Statement::Select(s)) => s,
///     _ => panic!("We should get an select statement")
/// };
///
/// assert!(s.locking.is_some());
/// println!("{:#?}", s.locking);
/// ```
#[derive(Debug, Clone)]
pub struct Select<'a> {
    /// Span of "SELECT"
    pub select_span: Span,
    /// Flags specified after "SELECT"
    pub flags: Vec<SelectFlag>,
    /// List of values to select
    pub select_exprs: Vec<SelectExpr<'a>>,
    /// Span of "FROM"
    pub from_span: Option<Span>,
    /// List of tables to select from
    pub table_references: Option<Vec<TableReference<'a>>>,
    /// Where expression and span of "WHERE" if specified
    pub where_: Option<(Expression<'a>, Span)>,
    /// Span of "GROUP_BY" and group expression if specified
    pub group_by: Option<(Span, Vec<Expression<'a>>)>,
    /// Having expression and span of "HAVING" if specified
    pub having: Option<(Expression<'a>, Span)>,
    /// Span of window if specified
    pub window_span: Option<Span>,
    /// Span of "ORDER BY" and list of order expression and directions, if specified
    pub order_by: Option<(Span, Vec<(Expression<'a>, OrderFlag)>)>,
    /// Span of "LIMIT", offset and count expressions if specified
    pub limit: Option<(Span, Option<Expression<'a>>, Expression<'a>)>,
    /// Row locking clause
    pub locking: Option<Locking<'a>>,
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
            .join_span(&self.having)
            .join_span(&self.window_span)
            .join_span(&self.order_by)
            .join_span(&self.limit)
    }
}

pub(crate) fn parse_select<'a>(parser: &mut Parser<'a, '_>) -> Result<Select<'a>, ParseError> {
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
                having: None,
                window_span: None,
                order_by: None,
                limit: None,
                locking: None,
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

    let having = if let Some(span) = parser.skip_keyword(Keyword::HAVING) {
        Some((parse_expression(parser, false)?, span))
    } else {
        None
    };

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

    let locking = if let Some(for_span) = parser.skip_keyword(Keyword::FOR) {
        let strength = match &parser.token {
            Token::Ident(_, Keyword::UPDATE) => {
                LockStrength::Update(parser.consume_keyword(Keyword::UPDATE)?)
            }
            Token::Ident(_, Keyword::SHARE) => {
                LockStrength::Share(parser.consume_keyword(Keyword::SHARE)?)
            }
            Token::Ident(_, Keyword::NO) => {
                LockStrength::NoKeyUpdate(parser.consume_keywords(&[
                    Keyword::NO,
                    Keyword::KEY,
                    Keyword::UPDATE,
                ])?)
            }
            Token::Ident(_, Keyword::KEY) => {
                LockStrength::KeyShare(parser.consume_keywords(&[Keyword::KEY, Keyword::SHARE])?)
            }
            _ => parser.expected_failure("UPDATE, SHARE, NO KEY UPDATE or KEY SHARE here")?,
        };

        if let LockStrength::NoKeyUpdate(s) | LockStrength::KeyShare(s) = &strength {
            if !parser.options.dialect.is_postgresql() {
                parser.add_error("Only support by PostgreSQL", s);
            }
        }

        let of = if let Some(of_span) = parser.skip_keyword(Keyword::OF) {
            let mut table_references = Vec::new();
            loop {
                table_references.push(parser.consume_plain_identifier()?);
                if parser.skip_token(Token::Comma).is_none() {
                    break;
                }
            }
            Some((of_span, table_references))
        } else {
            None
        };

        let wait = match &parser.token {
            Token::Ident(_, Keyword::NOWAIT) => {
                LockWait::NoWait(parser.consume_keyword(Keyword::NOWAIT)?)
            }
            Token::Ident(_, Keyword::SKIP) => {
                LockWait::SkipLocket(parser.consume_keywords(&[Keyword::SKIP, Keyword::LOCKED])?)
            }
            _ => LockWait::Default,
        };
        Some(Locking {
            for_span,
            strength,
            of,
            wait,
        })
    } else {
        None
    };

    // TODO [into_option]
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
        having,
        window_span,
        order_by,
        limit,
        locking,
    })
}
