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

use crate::{
    expression::{parse_expression, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    Span,
};

#[derive(Clone, Debug)]
pub enum UpdateFlag {
    LowPriority(Span),
    Ignore(Span),
}

#[derive(Clone, Debug)]
pub struct Update<'a> {
    pub update_span: Span,
    pub flags: Vec<UpdateFlag>,
    pub tables: Vec<Vec<(&'a str, Span)>>,
    pub set_span: Span,
    pub set: Vec<((&'a str, Span), Expression<'a>)>,
    pub where_: Option<(Expression<'a>, Span)>,
}

pub(crate) fn parse_update<'a>(parser: &mut Parser<'a>) -> Result<Update<'a>, ParseError> {
    let update_span = parser.consume_keyword(Keyword::UPDATE)?;
    let mut flags = Vec::new();

    loop {
        match &parser.token {
            Token::Ident(_, Keyword::LOW_PRIORITY) => flags.push(UpdateFlag::LowPriority(
                parser.consume_keyword(Keyword::LOW_PRIORITY)?,
            )),
            Token::Ident(_, Keyword::IGNORE) => {
                flags.push(UpdateFlag::Ignore(parser.consume_keyword(Keyword::IGNORE)?))
            }
            _ => break,
        }
    }

    let mut tables = Vec::new();
    loop {
        let mut table = vec![parser.consume_plain_identifier()?];
        loop {
            if parser.skip_token(Token::Period).is_none() {
                break;
            }
            table.push(parser.consume_plain_identifier()?);
        }
        tables.push(table);
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }

    let set_span = parser.consume_keyword(Keyword::SET)?;
    let mut set = Vec::new();
    loop {
        let col = parser.consume_plain_identifier()?;
        parser.consume_token(Token::Eq)?;
        let val = parse_expression(parser, false)?;
        set.push((col, val));
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }

    let where_ = if let Some(span) = parser.skip_keyword(Keyword::WHERE) {
        Some((parse_expression(parser, false)?, span))
    } else {
        None
    };

    Ok(Update {
        flags,
        update_span,
        tables,
        set_span,
        set,
        where_,
    })
}

// UPDATE [LOW_PRIORITY] [IGNORE] table_references
// SET col1={expr1|DEFAULT} [, col2={expr2|DEFAULT}] ...
// [WHERE where_condition]
