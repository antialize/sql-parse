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
use alloc::vec::Vec;

use crate::{
    expression::{parse_expression, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    qualified_name::parse_qualified_name,
    select::{parse_select, parse_select_expr, Select, SelectExpr},
    Identifier, Issue, OptSpanned, QualifiedName, Span, Spanned,
};

/// Flags for insert
#[derive(Clone, Debug)]
pub enum InsertReplaceFlag {
    LowPriority(Span),
    HighPriority(Span),
    Delayed(Span),
    Ignore(Span),
}

impl Spanned for InsertReplaceFlag {
    fn span(&self) -> Span {
        match &self {
            InsertReplaceFlag::LowPriority(v) => v.span(),
            InsertReplaceFlag::HighPriority(v) => v.span(),
            InsertReplaceFlag::Delayed(v) => v.span(),
            InsertReplaceFlag::Ignore(v) => v.span(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum InsertReplaceType {
    Insert(Span),
    Replace(Span),
}

impl Spanned for InsertReplaceType {
    fn span(&self) -> Span {
        match self {
            InsertReplaceType::Insert(a) => a.clone(),
            InsertReplaceType::Replace(a) => a.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum OnConflictTarget<'a> {
    Column {
        name: Identifier<'a>,
    },
    OnConstraint {
        on_constraint_span: Span,
        name: Identifier<'a>,
    },
    None,
}

impl<'a> OptSpanned for OnConflictTarget<'a> {
    fn opt_span(&self) -> Option<Span> {
        match self {
            OnConflictTarget::Column { name } => Some(name.span()),
            OnConflictTarget::OnConstraint {
                on_constraint_span: token,
                name,
            } => Some(token.join_span(name)),
            OnConflictTarget::None => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum OnConflictAction<'a> {
    DoNothing(Span),
    DoUpdateSet {
        do_update_set_span: Span,
        sets: Vec<(Identifier<'a>, Expression<'a>)>,
        where_: Option<(Span, alloc::boxed::Box<Expression<'a>>)>,
    },
}

impl<'a> Spanned for OnConflictAction<'a> {
    fn span(&self) -> Span {
        match self {
            OnConflictAction::DoNothing(span) => span.span(),
            OnConflictAction::DoUpdateSet {
                do_update_set_span,
                sets,
                where_,
            } => do_update_set_span.join_span(sets).join_span(where_),
        }
    }
}

#[derive(Clone, Debug)]
pub struct OnConflict<'a> {
    pub on_conflict_span: Span,
    pub target: OnConflictTarget<'a>,
    pub action: OnConflictAction<'a>,
}

impl<'a> Spanned for OnConflict<'a> {
    fn span(&self) -> Span {
        self.on_conflict_span
            .join_span(&self.target)
            .join_span(&self.action)
    }
}

#[derive(Clone, Debug)]
pub struct InsertReplaceSetPair<'a> {
    pub column: Identifier<'a>,
    pub equal_span: Span,
    pub value: Expression<'a>,
}

impl<'a> Spanned for InsertReplaceSetPair<'a> {
    fn span(&self) -> Span {
        self.column
            .join_span(&self.equal_span)
            .join_span(&self.value)
    }
}

#[derive(Clone, Debug)]
pub struct InsertReplaceSet<'a> {
    pub set_span: Span,
    pub pairs: Vec<InsertReplaceSetPair<'a>>,
}

impl<'a> Spanned for InsertReplaceSet<'a> {
    fn span(&self) -> Span {
        self.set_span.join_span(&self.pairs)
    }
}

#[derive(Clone, Debug)]
pub struct InsertReplaceOnDuplicateKeyUpdate<'a> {
    pub on_duplicate_key_update_span: Span,
    pub pairs: Vec<InsertReplaceSetPair<'a>>,
}

impl<'a> Spanned for InsertReplaceOnDuplicateKeyUpdate<'a> {
    fn span(&self) -> Span {
        self.on_duplicate_key_update_span.join_span(&self.pairs)
    }
}

/// Representation of Insert or Replace Statement
///
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statement, InsertReplace, InsertReplaceType, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql1 = "INSERT INTO person (first_name, last_name) VALUES ('John', 'Doe')";
/// let stmt1 = parse_statement(sql1, &mut issues, &options);
/// let sql2 = "INSERT INTO contractor SELECT * FROM person WHERE status = 'c'";
/// let stmt2 = parse_statement(sql2, &mut issues, &options);
/// let sql3 = "INSERT INTO account (`key`, `value`) VALUES ('foo', 42)
///             ON DUPLICATE KEY UPDATE `value`=`value`+42";
/// let stmt3 = parse_statement(sql3, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let i: InsertReplace = match stmt1 {
///     Some(Statement::InsertReplace(
///         i @ InsertReplace{type_: InsertReplaceType::Insert(_), ..})) => i,
///     _ => panic!("We should get an insert statement")
/// };
///
/// assert!(i.table.identifier.as_str() == "person");
/// println!("{:#?}", i.values.unwrap());
///
///
/// let sql = "REPLACE INTO t2 VALUES (1,'Leopard'),(2,'Dog')";
/// let stmt = parse_statement(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let r: InsertReplace = match stmt {
///     Some(Statement::InsertReplace(
///         r @ InsertReplace{type_: InsertReplaceType::Replace(_), ..})) => r,
///     _ => panic!("We should get an replace statement")
/// };
///
/// assert!(r.table.identifier.as_str() == "t2");
/// println!("{:#?}", r.values.unwrap());
/// ```
///
/// PostgreSQL
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statement, InsertReplace, InsertReplaceType, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::PostgreSQL).arguments(SQLArguments::Dollar);
/// # let mut issues = Vec::new();
/// #
///
/// let sql4 = "INSERT INTO contractor SELECT * FROM person WHERE status = $1 ON CONFLICT (name) DO NOTHING";
/// let stmt4 = parse_statement(sql4, &mut issues, &options);
///
/// for issue in &issues {
///     println!("{:#?}", issue);
/// }
/// # assert!(issues.is_empty());
/// ```
#[derive(Clone, Debug)]
pub struct InsertReplace<'a> {
    /// Span of "INSERT" or "REPLACE"
    pub type_: InsertReplaceType,
    /// Flags specified after "INSERT"
    pub flags: Vec<InsertReplaceFlag>,
    /// Span of "INTO" if specified
    pub into_span: Option<Span>,
    /// Table to insert into
    pub table: QualifiedName<'a>,
    /// List of columns to set
    pub columns: Vec<Identifier<'a>>,
    /// Span of values "VALUES" and list of tuples to insert if specified
    pub values: Option<(Span, Vec<Vec<Expression<'a>>>)>,
    /// Select statement to insert if specified
    pub select: Option<Select<'a>>,
    /// Span of "SET" and list of key, value pairs to set if specified
    pub set: Option<InsertReplaceSet<'a>>,
    /// Updates to execute on duplicate key (mysql)
    pub on_duplicate_key_update: Option<InsertReplaceOnDuplicateKeyUpdate<'a>>,
    /// Action to take on duplicate keys (postgresql)
    pub on_conflict: Option<OnConflict<'a>>,
    /// Span of "RETURNING" and select expressions after "RETURNING", if "RETURNING" is present
    pub returning: Option<(Span, Vec<SelectExpr<'a>>)>,
}

impl<'a> Spanned for InsertReplace<'a> {
    fn span(&self) -> Span {
        self.type_
            .join_span(&self.flags)
            .join_span(&self.into_span)
            .join_span(&self.table)
            .join_span(&self.values)
            .join_span(&self.select)
            .join_span(&self.set)
            .join_span(&self.on_duplicate_key_update)
            .join_span(&self.on_conflict)
            .join_span(&self.returning)
    }
}

pub(crate) fn parse_insert_replace<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<InsertReplace<'a>, ParseError> {
    let type_ = match &parser.token {
        Token::Ident(_, Keyword::INSERT) => InsertReplaceType::Insert(parser.consume()),
        Token::Ident(_, Keyword::REPLACE) => InsertReplaceType::Replace(parser.consume()),
        _ => parser.expected_failure("INSERT or REPLACE")?,
    };

    let insert = matches!(type_, InsertReplaceType::Insert(_));

    let mut flags = Vec::new();
    loop {
        match &parser.token {
            Token::Ident(_, Keyword::LOW_PRIORITY) => flags.push(InsertReplaceFlag::LowPriority(
                parser.consume_keyword(Keyword::LOW_PRIORITY)?,
            )),
            Token::Ident(_, Keyword::HIGH_PRIORITY) => flags.push(InsertReplaceFlag::HighPriority(
                parser.consume_keyword(Keyword::HIGH_PRIORITY)?,
            )),
            Token::Ident(_, Keyword::DELAYED) => flags.push(InsertReplaceFlag::Delayed(
                parser.consume_keyword(Keyword::DELAYED)?,
            )),
            Token::Ident(_, Keyword::IGNORE) => flags.push(InsertReplaceFlag::Ignore(
                parser.consume_keyword(Keyword::IGNORE)?,
            )),
            _ => break,
        }
    }

    for flag in &flags {
        match flag {
            InsertReplaceFlag::LowPriority(_) => {}
            InsertReplaceFlag::HighPriority(s) => {
                if !insert {
                    parser.add_error("Not supported for replace", s);
                }
            }
            InsertReplaceFlag::Delayed(_) => {}
            InsertReplaceFlag::Ignore(s) => {
                if !insert {
                    parser.add_error("Not supported for replace", s);
                }
            }
        }
    }

    let into_span = parser.skip_keyword(Keyword::INTO);
    let table = parse_qualified_name(parser)?;
    // [PARTITION (partition_list)]

    let mut columns = Vec::new();
    if parser.skip_token(Token::LParen).is_some() {
        parser.recovered(")", &|t| t == &Token::RParen, |parser| {
            loop {
                columns.push(parser.consume_plain_identifier()?);
                if parser.skip_token(Token::Comma).is_none() {
                    break;
                }
            }
            Ok(())
        })?;
        parser.consume_token(Token::RParen)?;
    }

    let mut select = None;
    let mut values = None;
    let mut set = None;
    match &parser.token {
        Token::Ident(_, Keyword::SELECT) => {
            select = Some(parse_select(parser)?);
        }
        Token::Ident(_, Keyword::VALUE | Keyword::VALUES) => {
            let values_span = parser.consume();
            let mut values_items = Vec::new();
            loop {
                let mut vals = Vec::new();
                parser.consume_token(Token::LParen)?;
                parser.recovered(")", &|t| t == &Token::RParen, |parser| {
                    loop {
                        vals.push(parse_expression(parser, false)?);
                        if parser.skip_token(Token::Comma).is_none() {
                            break;
                        }
                    }
                    Ok(())
                })?;
                parser.consume_token(Token::RParen)?;
                values_items.push(vals);
                if parser.skip_token(Token::Comma).is_none() {
                    break;
                }
            }
            values = Some((values_span, values_items));
        }
        Token::Ident(_, Keyword::SET) => {
            let set_span = parser.consume_keyword(Keyword::SET)?;
            let mut pairs = Vec::new();
            loop {
                let column = parser.consume_plain_identifier()?;
                let equal_span = parser.consume_token(Token::Eq)?;
                let value: Expression<'_> = parse_expression(parser, false)?;
                pairs.push(InsertReplaceSetPair {
                    column,
                    equal_span,
                    value,
                });
                if parser.skip_token(Token::Comma).is_none() {
                    break;
                }
            }
            if let Some(cs) = columns.opt_span() {
                parser.issues.push(
                    Issue::err(
                        "Columns may not be used here",
                        &cs,
                        &parser.sql_segment(cs.span()),
                    )
                    .frag(
                        "Together with SET",
                        &set_span,
                        &parser.sql_segment(set_span.span()),
                    ),
                );
            }
            set = Some(InsertReplaceSet { set_span, pairs });
        }
        _ => {
            parser.expected_error("VALUE, VALUES, SELECT or SET");
        }
    }

    let (on_duplicate_key_update, on_conflict) =
        if matches!(parser.token, Token::Ident(_, Keyword::ON)) {
            let on = parser.consume_keyword(Keyword::ON)?;
            match &parser.token {
                Token::Ident(_, Keyword::DUPLICATE) => {
                    let on_duplicate_key_update_span =
                        on.join_span(&parser.consume_keywords(&[
                            Keyword::DUPLICATE,
                            Keyword::KEY,
                            Keyword::UPDATE,
                        ])?);
                    let mut pairs = Vec::new();
                    loop {
                        let column = parser.consume_plain_identifier()?;
                        let equal_span = parser.consume_token(Token::Eq)?;
                        let value = parse_expression(parser, false)?;
                        pairs.push(InsertReplaceSetPair {
                            column,
                            equal_span,
                            value,
                        });
                        if parser.skip_token(Token::Comma).is_none() {
                            break;
                        }
                    }
                    if !parser.options.dialect.is_maria() {
                        parser.add_error(
                            "Only support by mariadb",
                            &on_duplicate_key_update_span.join_span(&pairs),
                        );
                    }
                    (
                        Some(InsertReplaceOnDuplicateKeyUpdate {
                            on_duplicate_key_update_span,
                            pairs,
                        }),
                        None,
                    )
                }
                Token::Ident(_, Keyword::CONFLICT) => {
                    let on_conflict_span =
                        on.join_span(&parser.consume_keyword(Keyword::CONFLICT)?);

                    let target = match &parser.token {
                        Token::LParen => {
                            parser.consume_token(Token::LParen)?;
                            let name = parser.consume_plain_identifier()?;
                            parser.consume_token(Token::RParen)?;
                            OnConflictTarget::Column { name }
                        }
                        Token::Ident(_, Keyword::ON) => {
                            let on_constraint =
                                parser.consume_keywords(&[Keyword::ON, Keyword::CONSTRAINT])?;
                            let name = parser.consume_plain_identifier()?;
                            OnConflictTarget::OnConstraint {
                                on_constraint_span: on_constraint,
                                name,
                            }
                        }
                        _ => OnConflictTarget::None,
                    };

                    let do_ = parser.consume_keyword(Keyword::DO)?;
                    let action = match &parser.token {
                        Token::Ident(_, Keyword::NOTHING) => OnConflictAction::DoNothing(
                            do_.join_span(&parser.consume_keyword(Keyword::NOTHING)?),
                        ),
                        Token::Ident(_, Keyword::UPDATE) => {
                            let do_update_set_span = do_.join_span(
                                &parser.consume_keywords(&[Keyword::UPDATE, Keyword::SET])?,
                            );
                            let mut sets = Vec::new();
                            loop {
                                let name = parser.consume_plain_identifier()?;
                                parser.consume_token(Token::Eq)?;
                                let expr = parse_expression(parser, false)?;
                                sets.push((name, expr));
                                if parser.skip_token(Token::Comma).is_none() {
                                    break;
                                }
                            }
                            let where_ = if matches!(parser.token, Token::Ident(_, Keyword::WHERE))
                            {
                                let where_span = parser.consume_keyword(Keyword::WHERE)?;
                                let where_expr =
                                    alloc::boxed::Box::new(parse_expression(parser, false)?);
                                Some((where_span, where_expr))
                            } else {
                                None
                            };
                            OnConflictAction::DoUpdateSet {
                                do_update_set_span,
                                sets,
                                where_,
                            }
                        }
                        _ => parser.expected_failure("'NOTHING' or 'UPDATE'")?,
                    };

                    let on_conflict = OnConflict {
                        on_conflict_span,
                        target,
                        action,
                    };

                    if !parser.options.dialect.is_postgresql() {
                        parser.add_error("Only support by postgesql", &on_conflict);
                    }

                    (None, Some(on_conflict))
                }
                _ => parser.expected_failure("'DUPLICATE' OR 'CONFLICT'")?,
            }
        } else {
            (None, None)
        };

    let returning = if let Some(returning_span) = parser.skip_keyword(Keyword::RETURNING) {
        let mut returning_exprs = Vec::new();
        loop {
            returning_exprs.push(parse_select_expr(parser)?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Some((returning_span, returning_exprs))
    } else {
        None
    };

    Ok(InsertReplace {
        type_,
        flags,
        table,
        columns,
        into_span,
        values,
        select,
        set,
        on_duplicate_key_update,
        on_conflict,
        returning,
    })
}
