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
    PrimaryKey(Span),
    As((Span, Box<Expression<'a>>)),
    Check((Span, Box<Expression<'a>>)),
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
            DataTypeProperty::Check((s, v)) => s.join_span(v),
            DataTypeProperty::PrimaryKey(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Timestamp {
    pub width: Option<(usize, Span)>,
    pub with_time_zone: Option<Span>,
}

impl OptSpanned for Timestamp {
    fn opt_span(&self) -> Option<Span> {
        self.width.opt_span().opt_join_span(&self.with_time_zone)
    }
}

/// Type of datatype
#[derive(Debug, Clone)]
pub enum Type<'a> {
    Boolean,
    TinyInt(Option<(usize, Span)>),
    SmallInt(Option<(usize, Span)>),
    Integer(Option<(usize, Span)>),
    Int(Option<(usize, Span)>),
    BigInt(Option<(usize, Span)>),
    Char(Option<(usize, Span)>),
    VarChar(Option<(usize, Span)>),
    TinyText(Option<(usize, Span)>),
    MediumText(Option<(usize, Span)>),
    Text(Option<(usize, Span)>),
    LongText(Option<(usize, Span)>),
    Enum(Vec<SString<'a>>),
    Set(Vec<SString<'a>>),
    Float8,
    Float(Option<(usize, usize, Span)>),
    Double(Option<(usize, usize, Span)>),
    Numeric(usize, usize, Span),
    DateTime(Option<(usize, Span)>),
    Timestamp(Timestamp),
    Timestamptz,
    Time(Option<(usize, Span)>),
    TinyBlob(Option<(usize, Span)>),
    MediumBlob(Option<(usize, Span)>),
    Date,
    Blob(Option<(usize, Span)>),
    LongBlob(Option<(usize, Span)>),
    VarBinary((usize, Span)),
    Binary(Option<(usize, Span)>),
    Named(Span),
    Json,
    Bit(usize, Span),
    Bytea,
    Inet4,
    Inet6,
}

impl<'a> OptSpanned for Type<'a> {
    fn opt_span(&self) -> Option<Span> {
        match &self {
            Type::Boolean => None,
            Type::TinyInt(v) => v.opt_span(),
            Type::SmallInt(v) => v.opt_span(),
            Type::Integer(v) => v.opt_span(),
            Type::Int(v) => v.opt_span(),
            Type::BigInt(v) => v.opt_span(),
            Type::Char(v) => v.opt_span(),
            Type::VarChar(v) => v.opt_span(),
            Type::TinyText(v) => v.opt_span(),
            Type::MediumText(v) => v.opt_span(),
            Type::Text(v) => v.opt_span(),
            Type::LongText(v) => v.opt_span(),
            Type::Enum(v) => v.opt_span(),
            Type::Set(v) => v.opt_span(),
            Type::Float8 => None,
            Type::Float(v) => v.opt_span(),
            Type::Double(v) => v.opt_span(),
            Type::Numeric(_, _, v) => v.opt_span(),
            Type::DateTime(v) => v.opt_span(),
            Type::Timestamp(v) => v.opt_span(),
            Type::Time(v) => v.opt_span(),
            Type::TinyBlob(v) => v.opt_span(),
            Type::MediumBlob(v) => v.opt_span(),
            Type::Date => None,
            Type::Blob(v) => v.opt_span(),
            Type::LongBlob(v) => v.opt_span(),
            Type::VarBinary(v) => v.opt_span(),
            Type::Binary(v) => v.opt_span(),
            Type::Timestamptz => None,
            Type::Named(v) => v.opt_span(),
            Type::Json => None,
            Type::Bit(_, b) => b.opt_span(),
            Type::Bytea => None,
            Type::Inet4 => None,
            Type::Inet6 => None,
        }
    }
}

/// Type of data
#[derive(Debug, Clone)]
pub struct DataType<'a> {
    /// Span of type_ identifier
    pub identifier: Span,
    /// Type with width
    pub type_: Type<'a>,
    /// Properties on type
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

fn parse_enum_set_values<'a>(parser: &mut Parser<'a, '_>) -> Result<Vec<SString<'a>>, ParseError> {
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

pub(crate) fn parse_data_type<'a>(
    parser: &mut Parser<'a, '_>,
    no_as: bool,
) -> Result<DataType<'a>, ParseError> {
    let (identifier, type_) = match &parser.token {
        Token::Ident(_, Keyword::BOOLEAN) => {
            (parser.consume_keyword(Keyword::BOOLEAN)?, Type::Boolean)
        }
        Token::Ident(_, Keyword::TINYINT) => (
            parser.consume_keyword(Keyword::TINYINT)?,
            Type::TinyInt(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::SMALLINT) => (
            parser.consume_keyword(Keyword::SMALLINT)?,
            Type::SmallInt(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::INTEGER) => (
            parser.consume_keyword(Keyword::INTEGER)?,
            Type::Integer(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::INT) => (
            parser.consume_keyword(Keyword::INT)?,
            Type::Int(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::BIGINT) => (
            parser.consume_keyword(Keyword::BIGINT)?,
            Type::BigInt(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::INET4) => (parser.consume_keyword(Keyword::INET4)?, Type::Inet4),
        Token::Ident(_, Keyword::INET6) => (parser.consume_keyword(Keyword::INET6)?, Type::Inet6),
        Token::Ident(_, Keyword::TINYTEXT) => (
            parser.consume_keyword(Keyword::TINYTEXT)?,
            Type::TinyText(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::CHAR) => (
            parser.consume_keyword(Keyword::CHAR)?,
            Type::Char(parse_width(parser)?),
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
            Type::VarChar(parse_width(parser)?),
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
        Token::Ident(_, Keyword::BINARY) => (
            parser.consume_keyword(Keyword::BINARY)?,
            Type::Binary(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::FLOAT8) => {
            (parser.consume_keyword(Keyword::FLOAT8)?, Type::Float8)
        }
        Token::Ident(_, Keyword::REAL) => {
            let i = parser.consume_keyword(Keyword::REAL)?;
            if parser.options.dialect.is_sqlite() {
                (i, Type::Double(None))
            } else {
                (i, Type::Float(None))
            }
        }
        Token::Ident(_, Keyword::FLOAT) => {
            (parser.consume_keyword(Keyword::FLOAT)?, Type::Float(None)) // TODO
        }
        Token::Ident(_, Keyword::DOUBLE) => {
            let i = if parser.options.dialect.is_postgresql() {
                parser.consume_keywords(&[Keyword::DOUBLE, Keyword::PRECISION])?
            } else {
                parser.consume_keyword(Keyword::DOUBLE)?
            };
            (i, Type::Double(None)) // TODO
        }
        Token::Ident(_, Keyword::NUMERIC) => {
            let numeric = parser.consume_keyword(Keyword::NUMERIC)?;
            let left = parser.consume_token(Token::LParen)?;
            let (v1, s1) = parser.consume_int()?;
            let comma = parser.consume_token(Token::Comma)?;
            let (v2, s2) = parser.consume_int()?;
            let right = parser.consume_token(Token::RParen)?;
            (
                numeric,
                Type::Numeric(
                    v1,
                    v2,
                    left.join_span(&s1)
                        .join_span(&comma)
                        .join_span(&s2)
                        .join_span(&right),
                ),
            )
        }
        Token::Ident(_, Keyword::DATETIME) => (
            parser.consume_keyword(Keyword::DATETIME)?,
            Type::DateTime(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::TIME) => (
            parser.consume_keyword(Keyword::TIME)?,
            Type::Time(parse_width(parser)?),
        ),
        Token::Ident(_, Keyword::TIMESTAMPTZ) => (
            parser.consume_keyword(Keyword::TIMESTAMPTZ)?,
            Type::Timestamptz,
        ),
        Token::Ident(_, Keyword::TIMESTAMP) => {
            let timestamp_span = parser.consume_keyword(Keyword::TIMESTAMP)?;
            let width = parse_width(parser)?;
            let with_time_zone = match parser.skip_keyword(Keyword::WITH) {
                Some(with_span) => Some(
                    with_span.join_span(&parser.consume_keywords(&[Keyword::TIME, Keyword::ZONE])?),
                ),
                None => None,
            };
            let timestamp = Timestamp {
                width,
                with_time_zone,
            };
            (timestamp_span, Type::Timestamp(timestamp))
        }
        Token::Ident(_, Keyword::DATE) => (parser.consume_keyword(Keyword::DATE)?, Type::Date),
        Token::Ident(_, Keyword::ENUM) => (
            parser.consume_keyword(Keyword::ENUM)?,
            Type::Enum(parse_enum_set_values(parser)?),
        ),
        Token::Ident(_, Keyword::SET) => (
            parser.consume_keyword(Keyword::SET)?,
            Type::Set(parse_enum_set_values(parser)?),
        ),
        Token::Ident(_, Keyword::JSON) => (parser.consume_keyword(Keyword::JSON)?, Type::Json),
        Token::Ident(_, Keyword::BYTEA) => (parser.consume_keyword(Keyword::BYTEA)?, Type::Bytea),
        Token::Ident(_, Keyword::BIT) => {
            let t = parser.consume_keyword(Keyword::BIT)?;
            let (w, ws) = parse_width_req(parser)?;
            (t, Type::Bit(w, ws))
        }
        Token::Ident(_, _) if parser.options.dialect.is_postgresql() => {
            let name = parser.consume();
            (name.clone(), Type::Named(name))
        }
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
                if parser.options.dialect.is_postgresql() {
                    properties.push(DataTypeProperty::GeneratedAlways(parser.consume_keywords(
                        &[
                            Keyword::GENERATED,
                            Keyword::ALWAYS,
                            Keyword::AS,
                            Keyword::IDENTITY,
                        ],
                    )?))
                } else {
                    properties.push(DataTypeProperty::GeneratedAlways(
                        parser.consume_keywords(&[Keyword::GENERATED, Keyword::ALWAYS])?,
                    ))
                }
            }
            Token::Ident(_, Keyword::AS) if !no_as => {
                let span = parser.consume_keyword(Keyword::AS)?;
                let s1 = parser.consume_token(Token::LParen)?;
                let e = parser.recovered(")", &|t| t == &Token::RParen, |parser| {
                    Ok(Some(parse_expression(parser, false)?))
                })?;
                let s2 = parser.consume_token(Token::RParen)?;
                let e = e.unwrap_or_else(|| Expression::Invalid(s1.join_span(&s2)));
                properties.push(DataTypeProperty::As((span, Box::new(e))));
            }
            Token::Ident(_, Keyword::PRIMARY) => properties.push(DataTypeProperty::PrimaryKey(
                parser.consume_keywords(&[Keyword::PRIMARY, Keyword::KEY])?,
            )),
            Token::Ident(_, Keyword::CHECK) => {
                let span = parser.consume_keyword(Keyword::CHECK)?;
                let s1 = parser.consume_token(Token::LParen)?;
                let e = parser.recovered(")", &|t| t == &Token::RParen, |parser| {
                    Ok(Some(parse_expression(parser, false)?))
                })?;
                let s2 = parser.consume_token(Token::RParen)?;
                let e = e.unwrap_or_else(|| Expression::Invalid(s1.join_span(&s2)));
                properties.push(DataTypeProperty::Check((span, Box::new(e))));
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
