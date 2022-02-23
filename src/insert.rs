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
    Identifier, Span, Spanned,
};

/// Flags for insert
#[derive(Clone, Debug)]
pub enum InsertFlag {
    LowPriority(Span),
    HighPriority(Span),
    Delayed(Span),
    Ignore(Span),
}

impl Spanned for InsertFlag {
    fn span(&self) -> Span {
        match &self {
            InsertFlag::LowPriority(v) => v.span(),
            InsertFlag::HighPriority(v) => v.span(),
            InsertFlag::Delayed(v) => v.span(),
            InsertFlag::Ignore(v) => v.span(),
        }
    }
}

/// Representation of Insert Statement
///
/// ```
/// # use sql_ast::{SQLDialect, SQLArguments, ParseOptions, parse_statement, Insert, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql1 = "INSERT INTO person (first_name, last_name) VALUES ('John', 'Doe')";
/// let stmt1 = parse_statement(sql1, &mut issues, &options);
/// let sql2 = "INSERT INTO contractor SELECT * FROM person WHERE status = 'c'";
/// let stmt2 = parse_statement(sql1, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let i: Insert = match stmt1 {
///     Some(Statement::Insert(i)) => i,
///     _ => panic!("We should get an insert statement")
/// };
///
/// assert!(i.table[0].as_str() == "person");
/// println!("{:#?}", i.values.unwrap())
/// ```
#[derive(Clone, Debug)]
pub struct Insert<'a> {
    /// Span of "INSERT"
    pub insert_span: Span,
    /// Flags specified after "INSERT"
    pub flags: Vec<InsertFlag>,
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
}

impl<'a> Spanned for Insert<'a> {
    fn span(&self) -> Span {
        self.insert_span
            .join_span(&self.flags)
            .join_span(&self.into_span)
            .join_span(&self.table)
            .join_span(&self.values)
            .join_span(&self.select)
    }
}

pub(crate) fn parse_insert<'a, 'b>(parser: &mut Parser<'a, 'b>) -> Result<Insert<'a>, ParseError> {
    let insert_span = parser.consume_keyword(Keyword::INSERT)?;
    let mut flags = Vec::new();

    loop {
        match &parser.token {
            Token::Ident(_, Keyword::LOW_PRIORITY) => flags.push(InsertFlag::LowPriority(
                parser.consume_keyword(Keyword::LOW_PRIORITY)?,
            )),
            Token::Ident(_, Keyword::HIGH_PRIORITY) => flags.push(InsertFlag::HighPriority(
                parser.consume_keyword(Keyword::HIGH_PRIORITY)?,
            )),
            Token::Ident(_, Keyword::DELAYED) => flags.push(InsertFlag::Delayed(
                parser.consume_keyword(Keyword::DELAYED)?,
            )),
            Token::Ident(_, Keyword::IGNORE) => {
                flags.push(InsertFlag::Ignore(parser.consume_keyword(Keyword::IGNORE)?))
            }
            _ => break,
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
    if matches!(parser.token, Token::Ident(_, Keyword::SELECT)) {
        select = Some(parse_select(parser)?);
    } else {
        let values_span = match &parser.token {
            Token::Ident(_, Keyword::VALUE) => parser.consume_keyword(Keyword::VALUE)?,
            Token::Ident(_, Keyword::VALUES) => parser.consume_keyword(Keyword::VALUES)?,
            _ => parser.expected_failure("'VALUES'")?,
        };

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

    //  [ ON DUPLICATE KEY UPDATE
    //    col=expr
    //      [, col=expr] ... ] [RETURNING select_expr
    //       [, select_expr ...]]

    Ok(Insert {
        flags,
        insert_span,
        table,
        columns,
        into_span,
        values,
        select,
    })
}
