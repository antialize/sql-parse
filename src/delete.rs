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
    Span, Spanned,
};

#[derive(Clone, Debug)]
pub enum DeleteFlag {
    LowPriority(Span),
    Quick(Span),
    Ignore(Span),
}

impl Spanned for DeleteFlag {
    fn span(&self) -> Span {
        match &self {
            DeleteFlag::LowPriority(v) => v.span(),
            DeleteFlag::Quick(v) => v.span(),
            DeleteFlag::Ignore(v) => v.span(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Delete<'a> {
    pub delete_span: Span,
    pub flags: Vec<DeleteFlag>,
    pub from_span: Span,
    pub table: Vec<(&'a str, Span)>,
    pub where_: Option<(Expression<'a>, Span)>,
}

impl<'a> Spanned for Delete<'a> {
    fn span(&self) -> Span {
        self.delete_span
            .join_span(&self.flags)
            .join_span(&self.from_span)
            .join_span(&self.table)
            .join_span(&self.where_)
    }
}

pub(crate) fn parse_delete<'a>(parser: &mut Parser<'a>) -> Result<Delete<'a>, ParseError> {
    let delete_span = parser.consume_keyword(Keyword::DELETE)?;
    let mut flags = Vec::new();

    parser.recovered(
        "FROM",
        &|t| matches!(t, Token::Ident(_, Keyword::FROM)),
        |parser| {
            loop {
                match &parser.token {
                    Token::Ident(_, Keyword::LOW_PRIORITY) => flags.push(DeleteFlag::LowPriority(
                        parser.consume_keyword(Keyword::LOW_PRIORITY)?,
                    )),
                    Token::Ident(_, Keyword::QUICK) => {
                        flags.push(DeleteFlag::Quick(parser.consume_keyword(Keyword::QUICK)?))
                    }
                    Token::Ident(_, Keyword::IGNORE) => {
                        flags.push(DeleteFlag::Ignore(parser.consume_keyword(Keyword::IGNORE)?))
                    }
                    _ => break,
                }
            }
            Ok(())
        },
    )?;

    let from_span = parser.consume_keyword(Keyword::FROM)?;

    let mut table = vec![parser.consume_plain_identifier()?];
    loop {
        if parser.skip_token(Token::Period).is_none() {
            break;
        }
        table.push(parser.consume_plain_identifier()?);
    }

    //TODO [PARTITION (partition_list)]
    //TODO [FOR PORTION OF period FROM expr1 TO expr2]

    let where_ = if let Some(span) = parser.skip_keyword(Keyword::WHERE) {
        Some((parse_expression(parser, false)?, span))
    } else {
        None
    };
    //TODO [ORDER BY ...]
    //TODO LIMIT row_count]
    //TODO [RETURNING select_expr
    //TODO  [, select_expr ...]]

    Ok(Delete {
        flags,
        delete_span,
        table,
        from_span,
        where_,
    })
}
