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

use std::borrow::Cow;

use crate::{
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser, SingleQuotedString},
    Span, Spanned,
};

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Null(Span),
    String((Cow<'a, str>, Span)),
    Integer((u64, Span)),
    Float((f64, Span)),
    CurrentTimestamp(Span),
}

impl<'a> std::fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String((v, _)) => write!(f, "COMMENT {}", SingleQuotedString(v.as_ref())),
            Literal::Null(_) => f.write_str("NULL"),
            Literal::Integer((v, _)) => write!(f, "{}", v),
            Literal::Float((v, _)) => write!(f, "{}", v),
            Literal::CurrentTimestamp(_) => f.write_str("CURRENT_TIMESTAMP()"),
        }
    }
}

impl<'a> Spanned for Literal<'a> {
    fn span(&self) -> Span {
        match &self {
            Literal::Null(_) => todo!(),
            Literal::String(_) => todo!(),
            Literal::Integer(_) => todo!(),
            Literal::Float(_) => todo!(),
            Literal::CurrentTimestamp(_) => todo!(),
        }
    }
}

pub(crate) fn parse_literal<'a>(parser: &mut Parser<'a>) -> Result<Literal<'a>, ParseError> {
    match &parser.token {
        Token::Ident(_, Keyword::NULL) => Ok(Literal::Null(parser.consume_keyword(Keyword::NULL)?)),
        Token::SingleQuotedString(_) => Ok(Literal::String(parser.consume_string()?)),
        Token::Integer(_) => Ok(Literal::Integer(parser.consume_int()?)),
        Token::Float(_) => Ok(Literal::Float(parser.consume_float()?)),
        Token::Ident(_, kw @ Keyword::CURRENT_TIMESTAMP) => {
            let kw = *kw;
            let s = parser.consume_keyword(kw)?;
            if parser.skip_token(Token::LParen).is_some() {
                parser.consume_token(Token::RParen)?;
            }
            Ok(Literal::CurrentTimestamp(s))
        }
        _ => parser.expected_failure("literal"),
    }
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
}

impl<'a> std::fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(v) => write!(f, "{}", v),
        }
    }
}

impl<'a> Spanned for Expression<'a> {
    fn span(&self) -> Span {
        match &self {
            Expression::Literal(v) => v.span(),
        }
    }
}
