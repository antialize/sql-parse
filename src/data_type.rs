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

use std::{
    borrow::Cow,
    fmt::{Display, Write},
    ops::Deref,
};

use crate::{
    expression::{parse_literal, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser, SingleQuotedString},
    Span, Spanned,
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
    Comment((Cow<'a, str>, Span)),
    Charset((&'a str, Span)),
    Collate((&'a str, Span)),
}

impl<'a> std::fmt::Display for DataTypeProperty<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataTypeProperty::Signed(_) => f.write_str("SIGNED"),
            DataTypeProperty::Unsigned(_) => f.write_str("UNSIGNED"),
            DataTypeProperty::Zerofill(_) => f.write_str("ZEROFILL"),
            DataTypeProperty::Null(_) => f.write_str("NULL"),
            DataTypeProperty::NotNull(_) => f.write_str("NOT NULL"),
            DataTypeProperty::Default(v) => match v.deref() {
                Expression::Literal(v) => write!(f, "DEFAULT {}", v),
            },
            DataTypeProperty::Comment((v, _)) => {
                write!(f, "COMMENT {}", SingleQuotedString(v.as_ref()))
            }
            DataTypeProperty::Charset((v, _)) => write!(f, "CHARSET {}", v),
            DataTypeProperty::Collate((v, _)) => write!(f, "COLLATE {}", v),
        }
    }
}

impl<'a> Spanned for DataTypeProperty<'a> {
    fn span(&self) -> Span {
        match &self {
            DataTypeProperty::Signed(s) => s.clone(),
            DataTypeProperty::Unsigned(s) => s.clone(),
            DataTypeProperty::Zerofill(s) => s.clone(),
            DataTypeProperty::Null(s) => s.clone(),
            DataTypeProperty::NotNull(s) => s.clone(),
            DataTypeProperty::Default(s) => s.span(),
            DataTypeProperty::Comment((_, s)) => s.clone(),
            DataTypeProperty::Charset((_, s)) => s.clone(),
            DataTypeProperty::Collate((_, s)) => s.clone(),
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
    Enum(Vec<(Cow<'a, str>, Span)>),
    Set(Vec<(Cow<'a, str>, Span)>),
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

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Type::TinyInt(v) => {
                f.write_str("TINYINT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::SmallInt(v) => {
                f.write_str("SMALLINT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::Int(v) => {
                f.write_str("INT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::BigInt(v) => {
                f.write_str("BIGINT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::VarChar((v, _)) => {
                write!(f, "VARCHAR({})", v)?;
            }
            Type::TinyText(v) => {
                f.write_str("TINYTEXT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::MediumText(v) => {
                f.write_str("MEDIUMTEXT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::Text(v) => {
                f.write_str("TEXT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::LongText(v) => {
                f.write_str("LONGTEXT")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::Enum(v) => {
                f.write_str("ENUM(")?;
                for (i, (v, _)) in v.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}", SingleQuotedString(v.as_ref()))?;
                }
                f.write_char(')')?;
            }
            Type::Set(v) => {
                f.write_str("SET(")?;
                for (i, (v, _)) in v.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}", SingleQuotedString(v.as_ref()))?;
                }
                f.write_char(')')?;
            }
            Type::Float(v) => {
                f.write_str("FLOAT")?;
                if let Some((v1, v2, _)) = v {
                    write!(f, "({}, {})", v1, v2)?;
                }
            }
            Type::Double(v) => {
                f.write_str("DOUBLE")?;
                if let Some((v1, v2, _)) = v {
                    write!(f, "({}, {})", v1, v2)?;
                }
            }
            Type::DateTime(v) => {
                f.write_str("DATETIME")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::Timestamp(v) => {
                f.write_str("TIMESTAMP")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::Time(v) => {
                f.write_str("TIME")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::TinyBlob(v) => {
                f.write_str("TINYBLOB")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::MediumBlob(v) => {
                f.write_str("MEDIUMBLOB")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::Date => {
                f.write_str("DATE")?;
            }
            Type::Blob(v) => {
                f.write_str("BLOB")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::LongBlob(v) => {
                f.write_str("LONGBLOB")?;
                if let Some((v, _)) = v {
                    write!(f, "({})", v)?;
                }
            }
            Type::VarBinary((v, _)) => {
                write!(f, "TINYINT({})", v)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct DataType<'a> {
    pub identifier: Span,
    pub type_: Type<'a>,
    pub properties: Vec<DataTypeProperty<'a>>,
}

impl<'a> Display for DataType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_)?;
        for p in &self.properties {
            write!(f, " {}", p)?;
        }
        Ok(())
    }
}

impl<'a> Spanned for DataType<'a> {
    fn span(&self) -> Span {
        self.properties
            .iter()
            .fold(self.identifier.span(), |a, b| a.join_span(b))
    }
}

fn parse_width(parser: &mut Parser<'_>) -> Result<Option<(usize, Span)>, ParseError> {
    if !matches!(parser.token, Token::LParen) {
        return Ok(None);
    }
    parser.consume_token(Token::LParen)?;
    let value = parser.recovered(")", &|t| t == &Token::RParen, |parser| parser.consume_int())?;
    parser.consume_token(Token::RParen)?;
    Ok(Some(value))
}

fn parse_width_req(parser: &mut Parser<'_>) -> Result<(usize, Span), ParseError> {
    if !matches!(parser.token, Token::LParen) {
        return parser.expected_failure("'('");
    }
    Ok(parse_width(parser)?.unwrap())
}

fn parse_enum_set_values<'a>(
    parser: &mut Parser<'a>,
) -> Result<Vec<(Cow<'a, str>, Span)>, ParseError> {
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

pub(crate) fn parse_data_type<'a>(parser: &mut Parser<'a>) -> Result<DataType<'a>, ParseError> {
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
                parser.consume_keyword(Keyword::CHARACTER)?;
                parser.consume_keyword(Keyword::SET)?;
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
                //TODO if '(' parse an expression
                properties.push(DataTypeProperty::Default(Box::new(Expression::Literal(
                    parse_literal(parser)?,
                ))));
            }
            //TODO default
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

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    use super::parse_data_type;

    #[test]
    fn datatype() {
        let src = "INT(44) NULL SIGNED DEFAULT 42 COMMENT 'monkey'";
        let mut parser = Parser::new(src);
        let ast = parse_data_type(&mut parser).unwrap();
        assert!(parser.issues.is_empty());
        assert_eq!(src, ast.to_string());
    }
}
