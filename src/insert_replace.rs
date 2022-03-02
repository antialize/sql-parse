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
use alloc::vec::Vec;

use crate::{
    expression::{parse_expression, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    select::{parse_select, Select},
    Identifier, Issue, OptSpanned, Span, Spanned,
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
/// assert!(i.table[0].as_str() == "person");
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
/// assert!(r.table[0].as_str() == "t2");
/// println!("{:#?}", r.values.unwrap());
///
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
    pub table: Vec<Identifier<'a>>,
    /// List of columns to set
    pub columns: Vec<Identifier<'a>>,
    /// Span of values "VALUES" and list of tuples to insert if specified
    pub values: Option<(Span, Vec<Vec<Expression<'a>>>)>,
    /// Select statement to insert if specified
    pub select: Option<Select<'a>>,
    /// Span of "SET" and list of key, value pairs to set if specified
    pub set: Option<(Span, Vec<(Identifier<'a>, Span, Expression<'a>)>)>,
    /// Updates to execute on duplicate key
    pub on_duplicate_key_update: Option<(Span, Vec<(Identifier<'a>, Span, Expression<'a>)>)>,
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
    }
}

pub(crate) fn parse_insert_replace<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
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
                    parser
                        .issues
                        .push(Issue::err("Not supported for replace", s));
                }
            }
            InsertReplaceFlag::Delayed(_) => {}
            InsertReplaceFlag::Ignore(s) => {
                if !insert {
                    parser
                        .issues
                        .push(Issue::err("Not supported for replace", s));
                }
            }
        }
    }

    let into_span = parser.skip_keyword(Keyword::INTO);
    let mut table = vec![parser.consume_plain_identifier()?];
    loop {
        if parser.skip_token(Token::Period).is_none() {
            break;
        }
        table.push(parser.consume_plain_identifier()?);
    }
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
            let mut kvps = Vec::new();
            loop {
                let col = parser.consume_plain_identifier()?;
                let eq_span = parser.consume_token(Token::Eq)?;
                let val = parse_expression(parser, false)?;
                kvps.push((col, eq_span, val));
                if parser.skip_token(Token::Comma).is_none() {
                    break;
                }
            }
            if let Some(cs) = columns.opt_span() {
                parser.issues.push(
                    Issue::err("Columns may not be used here", &cs)
                        .frag("Together with SET", &set_span),
                );
            }
            set = Some((set_span, kvps));
        }
        _ => {
            parser.expected_error("VALUE, VALUES, SELECT or SET");
        }
    }

    let on_duplicate_key_update = if matches!(parser.token, Token::Ident(_, Keyword::ON)) {
        let on_duplicate_key_update_span = parser.consume_keywords(&[
            Keyword::ON,
            Keyword::DUPLICATE,
            Keyword::KEY,
            Keyword::UPDATE,
        ])?;
        let mut kvps = Vec::new();
        loop {
            let col = parser.consume_plain_identifier()?;
            let eq_token = parser.consume_token(Token::Eq)?;
            let expr = parse_expression(parser, false)?;
            kvps.push((col, eq_token, expr));
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Some((on_duplicate_key_update_span, kvps))
    } else {
        None
    };

    //  [RETURNING select_expr
    //       [, select_expr ...]]
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
    })
}
