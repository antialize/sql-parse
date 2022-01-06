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
    data_type::{parse_data_type, DataType},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    Span, select::{Select, parse_select},
};

#[derive(Clone, Debug)]
pub enum TableOption<'a> {
    AutoExtendSize {
        identifier: Span,
        value: (&'a str, Span),
    },
    AutoIncrement {
        identifier: Span,
        value: (&'a str, Span),
    },
    AvgRowLength {
        identifier: Span,
        value: (&'a str, Span),
    },
    CharSet {
        identifier: Span,
        value: (&'a str, Span),
    },
    DefaultCharSet {
        identifier: Span,
        value: (&'a str, Span),
    },
    Checksum {
        identifier: Span,
        value: (bool, Span),
    },
    Collate {
        identifier: Span,
        value: (&'a str, Span),
    },
    DefaultCollate {
        identifier: Span,
        value: (&'a str, Span),
    },
    Comment {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    Compression {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    Connection {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    DataDirectory {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    IndexDirectory {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    DelayKeyWrite {
        identifier: Span,
        value: (bool, Span),
    },
    Encryption {
        identifier: Span,
        value: (bool, Span),
    },
    Engine {
        identifier: Span,
        value: (&'a str, Span),
    },
    EngineAttribute {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    InsertMethod {
        identifier: Span,
        value: (&'a str, Span),
    },
    KeyBlockSize {
        identifier: Span,
        value: (usize, Span),
    },
    MaxRows {
        identifier: Span,
        value: (usize, Span),
    },
    MinRows {
        identifier: Span,
        value: (usize, Span),
    },
    // PACK_KEYS
    Password {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    RowFormat {
        identifier: Span,
        value: (&'a str, Span),
    },
    SecondaryEngineAttribute {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    //StatsAutoRecalc
    //StatsPersistance
    //StatsSamplePages
    //TABLESPACE
    //UNION
}

#[derive(Clone, Debug)]
pub enum CreateDefinition<'a> {
    ColumnDefinition {
        identifier: (&'a str, Span),
        data_type: DataType<'a>,
    },
}

#[derive(Clone, Debug)]
pub struct CreateTable<'a> {
    pub create_span: Span,
    pub table_span: Span,
    pub identifier: (&'a str, Span),
    pub or_replace: Option<Span>,
    pub temporary: Option<Span>,
    pub if_not_exists: Option<Span>,
    pub create_definitions: Vec<CreateDefinition<'a>>,
    pub options: Vec<TableOption<'a>>,
}

pub(crate) fn parse_create_definition<'a>(
    parser: &mut Parser<'a>,
) -> Result<CreateDefinition<'a>, ParseError> {
    match &parser.token {
        Token::Ident(_, _) => Ok(CreateDefinition::ColumnDefinition {
            identifier: parser.consume_plain_identifier()?,
            data_type: parse_data_type(parser)?,
        }),
        _ => parser.expected_failure("identifier"),
    }
}

pub(crate) fn parse_create<'a>(parser: &mut Parser<'a>) -> Result<Statement<'a>, ParseError> {
    let create_span = parser.span.clone();
    parser.consume_keyword(Keyword::CREATE)?;

    let mut or_replace = None;
    let mut temporary = None;

    parser.recovered(
        "'TABLE'",
        &|t| matches!(t, Token::Ident(_, Keyword::TABLE)),
        |parser| {
            if let Some(or) = parser.skip_keyword(Keyword::OR) {
                or_replace = Some(or.start..parser.consume_keyword(Keyword::REPLACE)?.end);
            }
            temporary = parser.skip_keyword(Keyword::TEMPORARY);
            Ok(())
        },
    )?;
    let table_span = parser.consume_keyword(Keyword::TABLE)?;

    let mut identifier = ("", 0..0);
    let mut if_not_exists = None;

    parser.recovered("'('", &|t| t == &Token::LParen, |parser| {
        if let Some(if_) = parser.skip_keyword(Keyword::IF) {
            parser.consume_keyword(Keyword::NOT)?;
            if_not_exists = Some(if_.start..parser.consume_keyword(Keyword::EXISTS)?.end);
        }
        identifier = parser.consume_plain_identifier()?;
        Ok(())
    })?;

    parser.consume_token(Token::LParen)?;

    let mut create_definitions = Vec::new();
    loop {
        parser.recovered(
            "')' or ','",
            &|t| matches!(t, Token::RParen | Token::Comma),
            |parser| {
                create_definitions.push(parse_create_definition(parser)?);
                Ok(())
            },
        )?;
        if matches!(parser.token, Token::RParen) {
            break;
        }
        parser.consume_token(Token::Comma)?;
    }
    parser.consume_token(Token::RParen)?;

    let mut options = Vec::new();
    parser.recovered(
        ";",
        &|t| matches!(t, Token::SemiColon | Token::Eof),
        |parser| {
            loop {
                let identifier = parser.span.clone();
                match &parser.token {
                    Token::Ident(_, Keyword::ENGINE) => {
                        parser.consume_keyword(Keyword::ENGINE)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::Engine {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                    }
                    Token::Ident(_, Keyword::DEFAULT) => {
                        parser.consume_keyword(Keyword::DEFAULT)?;
                        match &parser.token {
                            Token::Ident(_, Keyword::CHARSET) => {
                                parser.consume_keyword(Keyword::CHARSET)?;
                                parser.skip_token(Token::Eq);
                                options.push(TableOption::DefaultCharSet {
                                    identifier,
                                    value: parser.consume_plain_identifier()?,
                                });
                            }
                            Token::Ident(_, Keyword::COLLATE) => {
                                parser.consume_keyword(Keyword::COLLATE)?;
                                parser.skip_token(Token::Eq);
                                options.push(TableOption::DefaultCollate {
                                    identifier,
                                    value: parser.consume_plain_identifier()?,
                                });
                            }
                            _ => parser.expected_failure("'CHARSET' or 'COLLATE'")?,
                        }
                    }
                    Token::Ident(_, Keyword::CHARSET) => {
                        parser.consume_keyword(Keyword::CHARSET)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::CharSet {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                    }
                    Token::Ident(_, Keyword::COLLATE) => {
                        parser.consume_keyword(Keyword::COLLATE)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::Collate {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                    }
                    Token::Ident(_, Keyword::ROW_FORMAT) => {
                        parser.consume_keyword(Keyword::ROW_FORMAT)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::RowFormat {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                        //TODO validate raw format is in the keyword set
                    }
                    Token::Ident(_, Keyword::COMMENT) => {
                        parser.consume_keyword(Keyword::COMMENT)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::Comment {
                            identifier,
                            value: parser.consume_string()?,
                        });
                    }
                    Token::SemiColon | Token::Eof => break,
                    _ => {
                        parser.expected_failure("table option or ';'")?;
                    }
                }
            }
            Ok(())
        },
    )?;

    Ok(Statement::CreateTable(CreateTable {
        create_span,
        table_span,
        identifier,
        or_replace,
        temporary,
        if_not_exists,
        options,
        create_definitions,
    }))
}

pub(crate) fn parse_drop<'a>(parser: &mut Parser<'a>) -> Result<Statement<'a>, ParseError> {
    parser.error("parse_drop not implemented")
}

#[derive(Clone, Debug)]
pub enum Statement<'a> {
    CreateTable(CreateTable<'a>),
    Select(Select<'a>)
}

pub(crate) fn parse_statement<'a>(parser: &mut Parser<'a>) -> Result<Statement<'a>, ParseError> {
    match &parser.token {
        Token::Ident(_, Keyword::CREATE) => parse_create(parser),
        Token::Ident(_, Keyword::DROP) => parse_drop(parser),
        Token::Ident(_, Keyword::SELECT) => Ok(Statement::Select(parse_select(parser)?)),
        _ => parser.expected_failure("Statement"),
    }
}

pub(crate) fn parse_statements<'a>(parser: &mut Parser<'a>) -> Vec<Statement<'a>> {
    let mut ans = Vec::new();
    loop {
        loop {
            match &parser.token {
                Token::SemiColon => {
                    parser.skip_token(Token::SemiColon);
                }
                Token::Eof => return ans,
                _ => break,
            }
        }

        let stmt = parse_statement(parser);
        let err = stmt.is_err();
        if let Ok(stmt) = stmt {
            ans.push(stmt);
        }

        if !matches!(parser.token, Token::SemiColon) {
            if !err {
                parser.expected_error(";");
            }
            // We use a custom recovery here as ; is not allowed in sub expressions, it always terminates outer most statements
            loop {
                parser.next();
                match &parser.token {
                    Token::SemiColon => break,
                    Token::Eof => return ans,
                    _ => (),
                }
            }
        }
        parser.consume_token(Token::SemiColon).unwrap();
    }
}
