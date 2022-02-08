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
    select::{parse_table_reference, TableReference},
    span::OptSpanned,
    Identifier, Span, Spanned,
};

#[derive(Clone, Debug)]
pub enum UpdateFlag {
    LowPriority(Span),
    Ignore(Span),
}

impl Spanned for UpdateFlag {
    fn span(&self) -> Span {
        match &self {
            UpdateFlag::LowPriority(v) => v.span(),
            UpdateFlag::Ignore(v) => v.span(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Update<'a> {
    pub update_span: Span,
    pub flags: Vec<UpdateFlag>,
    pub tables: Vec<TableReference<'a>>,
    pub set_span: Span,
    pub set: Vec<(Vec<Identifier<'a>>, Expression<'a>)>,
    pub where_: Option<(Expression<'a>, Span)>,
}

impl<'a> Spanned for Update<'a> {
    fn span(&self) -> Span {
        let mut set_span = None;
        for (a, b) in &self.set {
            set_span = set_span.opt_join_span(a).opt_join_span(b)
        }

        self.update_span
            .join_span(&self.flags)
            .join_span(&self.tables)
            .join_span(&self.set_span)
            .join_span(&set_span)
            .join_span(&self.where_)
    }
}

pub(crate) fn parse_update<'a, 'b>(parser: &mut Parser<'a, 'b>) -> Result<Update<'a>, ParseError> {
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
        tables.push(parse_table_reference(parser)?);
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }

    let set_span = parser.consume_keyword(Keyword::SET)?;
    let mut set = Vec::new();
    loop {
        let mut col = vec![parser.consume_plain_identifier()?];
        while parser.skip_token(Token::Period).is_some() {
            col.push(parser.consume_plain_identifier()?);
        }
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
