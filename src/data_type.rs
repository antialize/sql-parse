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

use crate::{
    expression::{parse_expression, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    span::OptSpanned,
    Identifier, SString, Span, Spanned,
};

/// A property on a datatype
#[derive(Debug, Clone)]
pub enum DataTypeProperty<'a> {
    Signed(Span),
    Unsigned(Span),
    Zerofill(Span),
    Null(Span),
    NotNull(Span),
    Default(Box<Expression<'a>>),
    Comment(SString<'a>),
    Charset(Identifier<'a>),
    Collate(Identifier<'a>),
    Virtual(Span),
    Persistent(Span),
    Stored(Span),
    Unique(Span),
    UniqueKey(Span),
    GeneratedAlways(Span),
    AutoIncrement(Span),
    As((Span, Box<Expression<'a>>)),
}

impl<'a> Spanned for DataTypeProperty<'a> {
    fn span(&self) -> Span {
        match &self {
            DataTypeProperty::Signed(v) => v.span(),
            DataTypeProperty::Unsigned(v) => v.span(),
            DataTypeProperty::Zerofill(v) => v.span(),
            DataTypeProperty::Null(v) => v.span(),
            DataTypeProperty::NotNull(v) => v.span(),
            DataTypeProperty::Default(v) => v.span(),
            DataTypeProperty::Comment(v) => v.span(),
            DataTypeProperty::Charset(v) => v.span(),
            DataTypeProperty::Collate(v) => v.span(),
            DataTypeProperty::Virtual(v) => v.span(),
            DataTypeProperty::Persistent(v) => v.span(),
            DataTypeProperty::Stored(v) => v.span(),
            DataTypeProperty::Unique(v) => v.span(),
            DataTypeProperty::UniqueKey(v) => v.span(),
            DataTypeProperty::GeneratedAlways(v) => v.span(),
            DataTypeProperty::AutoIncrement(v) => v.span(),
            DataTypeProperty::As((s, v)) => s.join_span(v),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
    TinyInt(Option<(usize, Span)>),
    SmallInt(Option<(usize, Span)>),
    Int(Option<(usize, Span)>),
    BigInt(Option<(usize, Span)>),
    VarChar((usize, Span)),
    TinyText(Option<(usize, Span)>),
    MediumText(Option<(usize, Span)>),
    Text(Option<(usize, Span)>),
    LongText(Option<(usize, Span)>),
    Enum(Vec<SString<'a>>),
    Set(Vec<SString<'a>>),
    Float(Option<(usize, usize, Span)>),
    Double(Option<(usize, usize, Span)>),
    DateTime(Option<(usize, Span)>),
    Timestamp(Option<(usize, Span)>),
    Time(Option<(usize, Span)>),
    TinyBlob(Option<(usize, Span)>),
    MediumBlob(Option<(usize, Span)>),
    Date,
    Blob(Option<(usize, Span)>),
    LongBlob(Option<(usize, Span)>),
    VarBinary((usize, Span)),
}

impl<'a> OptSpanned for Type<'a> {
    fn opt_span(&self) -> Option<Span> {
        match &self {
            Type::TinyInt(v) => v.opt_span(),
            Type::SmallInt(v) => v.opt_span(),
            Type::Int(v) => v.opt_span(),
            Type::BigInt(v) => v.opt_span(),
            Type::VarChar(v) => v.opt_span(),
            Type::TinyText(v) => v.opt_span(),
            Type::MediumText(v) => v.opt_span(),
            Type::Text(v) => v.opt_span(),
            Type::LongText(v) => v.opt_span(),
            Type::Enum(v) => v.opt_span(),
            Type::Set(v) => v.opt_span(),
            Type::Float(v) => v.opt_span(),
            Type::Double(v) => v.opt_span(),
            Type::DateTime(v) => v.opt_span(),
            Type::Timestamp(v) => v.opt_span(),
            Type::Time(v) => v.opt_span(),
            Type::TinyBlob(v) => v.opt_span(),
            Type::MediumBlob(v) => v.opt_span(),
            Type::Date => None,
            Type::Blob(v) => v.opt_span(),
            Type::LongBlob(v) => v.opt_span(),
            Type::VarBinary(v) => v.opt_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DataType<'a> {
    pub identifier: Span,
    pub type_: Type<'a>,
    pub properties: Vec<DataTypeProperty<'a>>,
}

impl<'a> Spanned for DataType<'a> {
    fn span(&self) -> Span {
        self.identifier
            .join_span(&self.type_)
            .join_span(&self.properties)
    }
}
fn parse_width(parser: &mut Parser<'_, '_>) -> Result<Option<(usize, Span)>, ParseError> {
    if !matches!(parser.token, Token::LParen) {
        return Ok(None);
    }
    parser.consume_token(Token::LParen)?;
    let value = parser.recovered(")", &|t| t == &Token::RParen, |parser| parser.consume_int())?;
    parser.consume_token(Token::RParen)?;
    Ok(Some(value))
}

fn parse_width_req(parser: &mut Parser<'_, '_>) -> Result<(usize, Span), ParseError> {
    if !matches!(parser.token, Token::LParen) {
        return parser.expected_failure("'('");
    }
    Ok(parse_width(parser)?.expect("width"))
}

fn parse_enum_set_values<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<Vec<SString<'a>>, ParseError> {
    parser.consume_token(Token::LParen)?;
    let mut ans = Vec::new();
    parser.recovered(")", &|t| t == &Token::RParen, |parser| {
        loop {
            ans.push(parser.consume_string()?);
            match &parser.token {
                Token::Comma => {
                    parser.consume_token(Token::Comma)?;
                }
                Token::RParen => break,
                _ => parser.expected_failure("',' or ')'")?,
            }
        }
        Ok(())
    })?;
    parser.consume_token(Token::RParen)?;
    Ok(ans)
}

pub(crate) fn parse_data_type<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<DataType<'a>, ParseError> {
    let (identifier, type_) = match &parser.token {
        Token::Ident(_, Keyword::TINYINT) => (
            parser.consume_keyword(Keyword::TINYINT)?,
            Type::TinyInt(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::SMALLINT) => (
            parser.consume_keyword(Keyword::SMALLINT)?,
            Type::SmallInt(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::INT) => (
            parser.consume_keyword(Keyword::INT)?,
            Type::Int(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::BIGINT) => (
            parser.consume_keyword(Keyword::BIGINT)?,
            Type::BigInt(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::TINYTEXT) => (
            parser.consume_keyword(Keyword::TINYTEXT)?,
            Type::TinyText(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::TEXT) => (
            parser.consume_keyword(Keyword::TEXT)?,
            Type::Text(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::MEDIUMTEXT) => (
            parser.consume_keyword(Keyword::MEDIUMTEXT)?,
            Type::MediumText(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::LONGTEXT) => (
            parser.consume_keyword(Keyword::LONGTEXT)?,
            Type::LongText(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::VARCHAR) => (
            parser.consume_keyword(Keyword::VARCHAR)?,
            Type::VarChar(parse_width_req(parser)?),
        ),
        Token::Ident(_, Keyword::TINYBLOB) => (
            parser.consume_keyword(Keyword::TINYBLOB)?,
            Type::TinyBlob(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::BLOB) => (
            parser.consume_keyword(Keyword::BLOB)?,
            Type::Blob(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::MEDIUMBLOB) => (
            parser.consume_keyword(Keyword::MEDIUMBLOB)?,
            Type::MediumBlob(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::LONGBLOB) => (
            parser.consume_keyword(Keyword::LONGBLOB)?,
            Type::LongBlob(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::VARBINARY) => (
            parser.consume_keyword(Keyword::VARBINARY)?,
            Type::VarBinary(parse_width_req(parser)?),
        ),
        Token::Ident(_, Keyword::FLOAT) => {
            (parser.consume_keyword(Keyword::FLOAT)?, Type::Float(None)) // TODO
        }
        Token::Ident(_, Keyword::DOUBLE) => {
            (parser.consume_keyword(Keyword::DOUBLE)?, Type::Double(None)) // TODO
        }
        Token::Ident(_, Keyword::DATETIME) => (
            parser.consume_keyword(Keyword::DATETIME)?,
            Type::DateTime(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::TIME) => (
            parser.consume_keyword(Keyword::TIME)?,
            Type::Time(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::TIMESTAMP) => (
            parser.consume_keyword(Keyword::TIMESTAMP)?,
            Type::Timestamp(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::DATE) => (parser.consume_keyword(Keyword::DATE)?, Type::Date),
        Token::Ident(_, Keyword::ENUM) => (
            parser.consume_keyword(Keyword::ENUM)?,
            Type::Enum(parse_enum_set_values(parser)?),
        ),
        Token::Ident(_, Keyword::SET) => (
            parser.consume_keyword(Keyword::SET)?,
            Type::Set(parse_enum_set_values(parser)?),
        ),
        _ => parser.expected_failure("type")?,
    };
    let mut properties = Vec::new();
    loop {
        match parser.token {
            Token::Ident(_, Keyword::SIGNED) => properties.push(DataTypeProperty::Signed(
                parser.consume_keyword(Keyword::SIGNED)?,
            )),
            Token::Ident(_, Keyword::AUTO_INCREMENT) => properties.push(
                DataTypeProperty::AutoIncrement(parser.consume_keyword(Keyword::AUTO_INCREMENT)?),
            ),
            Token::Ident(_, Keyword::UNSIGNED) => properties.push(DataTypeProperty::Unsigned(
                parser.consume_keyword(Keyword::UNSIGNED)?,
            )),
            Token::Ident(_, Keyword::ZEROFILL) => properties.push(DataTypeProperty::Zerofill(
                parser.consume_keyword(Keyword::ZEROFILL)?,
            )),
            Token::Ident(_, Keyword::NULL) => properties.push(DataTypeProperty::Null(
                parser.consume_keyword(Keyword::NULL)?,
            )),
            Token::Ident(_, Keyword::NOT) => {
                let start = parser.consume_keyword(Keyword::NOT)?.start;
                properties.push(DataTypeProperty::NotNull(
                    start..parser.consume_keyword(Keyword::NULL)?.end,
                ));
            }
            Token::Ident(_, Keyword::CHARACTER) => {
                parser.consume_keywords(&[Keyword::CHARACTER, Keyword::SET])?;
                properties.push(DataTypeProperty::Charset(
                    parser.consume_plain_identifier()?,
                ));
            }
            Token::Ident(_, Keyword::COLLATE) => {
                parser.consume_keyword(Keyword::COLLATE)?;
                properties.push(DataTypeProperty::Charset(
                    parser.consume_plain_identifier()?,
                ));
            }
            Token::Ident(_, Keyword::COMMENT) => {
                parser.consume_keyword(Keyword::COMMENT)?;
                properties.push(DataTypeProperty::Comment(parser.consume_string()?));
            }
            Token::Ident(_, Keyword::DEFAULT) => {
                parser.consume_keyword(Keyword::DEFAULT)?;
                properties.push(DataTypeProperty::Default(Box::new(parse_expression(
                    parser, true,
                )?)));
            }
            Token::Ident(_, Keyword::VIRTUAL) => properties.push(DataTypeProperty::Virtual(
                parser.consume_keyword(Keyword::VIRTUAL)?,
            )),
            Token::Ident(_, Keyword::PERSISTENT) => properties.push(DataTypeProperty::Persistent(
                parser.consume_keyword(Keyword::PERSISTENT)?,
            )),
            Token::Ident(_, Keyword::STORED) => properties.push(DataTypeProperty::Stored(
                parser.consume_keyword(Keyword::STORED)?,
            )),
            Token::Ident(_, Keyword::UNIQUE) => {
                let span = parser.consume_keyword(Keyword::UNIQUE)?;
                if let Some(s2) = parser.skip_keyword(Keyword::KEY) {
                    properties.push(DataTypeProperty::UniqueKey(s2.join_span(&span)));
                } else {
                    properties.push(DataTypeProperty::Unique(span));
                }
            }
            Token::Ident(_, Keyword::GENERATED) => {
                properties.push(DataTypeProperty::GeneratedAlways(
                    parser.consume_keywords(&[Keyword::GENERATED, Keyword::ALWAYS])?,
                ))
            }
            Token::Ident(_, Keyword::AS) => {
                let span = parser.consume_keyword(Keyword::AS)?;
                let s1 = parser.consume_token(Token::LParen)?;
                let e = parser.recovered(")", &|t| t == &Token::RParen, |parser| {
                    Ok(Some(parse_expression(parser, false)?))
                })?;
                let s2 = parser.consume_token(Token::RParen)?;
                let e = e.unwrap_or_else(|| Expression::Invalid(s1.join_span(&s2)));
                properties.push(DataTypeProperty::As((span, Box::new(e))));
            }
            _ => break,
        }
    }
    // TODO validate properties order
    // TODO validate allowed properties
    Ok(DataType {
        identifier,
        type_,
        properties,
    })
}
