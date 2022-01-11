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
    Span, Spanned, Statement,
};

#[derive(Clone, Debug)]
pub enum IndexOption<'a> {
    IndexTypeBTree(Span),
    IndexTypeHash(Span),
    IndexTypeRTree(Span),
    Dummy(&'a str),
}

#[derive(Clone, Debug)]
pub enum IndexType {
    Index(Span),
    Primary(Span),
    Unique(Span),
    FullText(Span),
    Spatial(Span),
}

#[derive(Clone, Debug)]
pub enum AlterSpecification<'a> {
    Dummy(&'a str),
    AddIndex {
        add_span: Span,
        index_type: IndexType,
        if_not_exists: Option<Span>,
        name: Option<(&'a str, Span)>,
        constraint: Option<(Span, Option<(&'a str, Span)>)>,
        cols: Vec<(&'a str, Span)>,
        index_options: Vec<IndexOption<'a>>,
    },
}

fn parse_index_type<'a>(
    parser: &mut Parser<'a>,
    out: &mut Vec<IndexOption<'a>>,
) -> Result<(), ParseError> {
    parser.consume_keyword(Keyword::USING)?;
    out.push(match &parser.token {
        Token::Ident(_, Keyword::BTREE) => {
            IndexOption::IndexTypeBTree(parser.consume_keyword(Keyword::BTREE)?)
        }
        Token::Ident(_, Keyword::HASH) => {
            IndexOption::IndexTypeHash(parser.consume_keyword(Keyword::HASH)?)
        }
        Token::Ident(_, Keyword::RTREE) => {
            IndexOption::IndexTypeRTree(parser.consume_keyword(Keyword::RTREE)?)
        }
        _ => parser.expected_failure("'BTREE', 'RTREE' or 'HASH'")?,
    });
    Ok(())
}

fn parse_index_options<'a>(
    parser: &mut Parser<'a>,
    out: &mut Vec<IndexOption<'a>>,
) -> Result<(), ParseError> {
    loop {
        match &parser.token {
            Token::Ident(_, Keyword::USING) => parse_index_type(parser, out)?,
            _ => break,
        }
    }
    Ok(())
}

fn parse_index_cols<'a>(parser: &mut Parser<'a>) -> Result<Vec<(&'a str, Span)>, ParseError> {
    parser.consume_token(Token::LParen)?;
    let mut ans = Vec::new();
    parser.recovered("')'", &|t| t == &Token::RParen, |parser| {
        loop {
            ans.push(parser.consume_plain_identifier()?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Ok(())
    })?;
    parser.consume_token(Token::RParen)?;
    Ok(ans)
}

fn parse_add_alter_specification<'a>(
    parser: &mut Parser<'a>,
) -> Result<AlterSpecification<'a>, ParseError> {
    let add_span = parser.consume_keyword(Keyword::ADD)?;
    let constraint = if let Some(span) = parser.skip_keyword(Keyword::CONSTRAINT) {
        let v = match &parser.token {
            Token::Ident(_, kw) if !kw.reserved() => Some(parser.consume_plain_identifier()?),
            _ => None,
        };
        Some((span, v))
    } else {
        None
    };
    match &parser.token {
        Token::Ident(
            _,
            Keyword::PRIMARY
            | Keyword::INDEX
            | Keyword::KEY
            | Keyword::FULLTEXT
            | Keyword::UNIQUE
            | Keyword::SPATIAL,
        ) => {
            let index_type = match &parser.token {
                Token::Ident(_, Keyword::PRIMARY) => IndexType::Primary(
                    parser
                        .consume_keyword(Keyword::PRIMARY)?
                        .join_span(&parser.consume_keyword(Keyword::KEY)?),
                ),
                Token::Ident(_, Keyword::INDEX | Keyword::KEY) => {
                    IndexType::Index(parser.consume())
                }
                Token::Ident(_, Keyword::FULLTEXT) => {
                    let s = parser.consume_keyword(Keyword::FULLTEXT)?;
                    match &parser.token {
                        Token::Ident(_, kw @ Keyword::INDEX | kw @ Keyword::KEY) => {
                            let kw = *kw;
                            IndexType::FullText(parser.consume_keyword(kw)?.join_span(&s))
                        }
                        _ => parser.expected_failure("'KEY' or 'INDEX'")?,
                    }
                }
                Token::Ident(_, Keyword::SPATIAL) => {
                    let s = parser.consume_keyword(Keyword::SPATIAL)?;
                    match &parser.token {
                        Token::Ident(_, kw @ Keyword::INDEX | kw @ Keyword::KEY) => {
                            let kw = *kw;
                            IndexType::FullText(parser.consume_keyword(kw)?.join_span(&s))
                        }
                        _ => parser.expected_failure("'KEY' or 'INDEX'")?,
                    }
                }
                Token::Ident(_, Keyword::UNIQUE) => {
                    let s = parser.consume_keyword(Keyword::UNIQUE)?;
                    match &parser.token {
                        Token::Ident(_, kw @ Keyword::INDEX | kw @ Keyword::KEY) => {
                            let kw = *kw;
                            IndexType::FullText(parser.consume_keyword(kw)?.join_span(&s))
                        }
                        _ => parser.expected_failure("'KEY' or 'INDEX'")?,
                    }
                }
                _ => parser.error("ICE")?,
            };

            let if_not_exists = if let Some(s) = parser.skip_keyword(Keyword::IF) {
                Some(
                    parser
                        .consume_keyword(Keyword::NOT)?
                        .join_span(&parser.consume_keyword(Keyword::EXISTS)?)
                        .join_span(&s),
                )
            } else {
                None
            };

            let name = match &parser.token {
                Token::Ident(_, kw) if !kw.reserved() => Some(parser.consume_plain_identifier()?),
                _ => None,
            };

            let mut index_options = Vec::new();
            if matches!(parser.token, Token::Ident(_, Keyword::USING)) {
                parse_index_type(parser, &mut index_options)?;
            }
            let cols = parse_index_cols(parser)?;
            parse_index_options(parser, &mut index_options)?;

            Ok(AlterSpecification::AddIndex {
                add_span,
                constraint,
                index_type,
                if_not_exists,
                name,
                cols,
                index_options,
            })
        }
        _ => parser.expected_failure("addable"),
    }
}

#[derive(Clone, Debug)]
pub struct AlterTable<'a> {
    pub alter_span: Span,
    pub online: Option<Span>,
    pub ignore: Option<Span>,
    pub table_span: Span,
    pub if_exists: Option<Span>,
    pub table: (&'a str, Span),
    pub alter_specifications: Vec<AlterSpecification<'a>>,
}

fn parse_alter_table<'a>(
    parser: &mut Parser<'a>,
    alter_span: Span,
    online: Option<Span>,
    ignore: Option<Span>,
) -> Result<AlterTable<'a>, ParseError> {
    let table_span = parser.consume_keyword(Keyword::TABLE)?;
    let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
        Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
    } else {
        None
    };
    let table = parser.consume_plain_identifier()?;
    let d = parser.delimiter.clone();
    let mut alter_specifications = Vec::new();
    parser.recovered(d.name(), &|t| t == &d, |parser| {
        loop {
            alter_specifications.push(match parser.token {
                Token::Ident(_, Keyword::ADD) => parse_add_alter_specification(parser)?,
                _ => parser.expected_failure("alter specification")?,
            });
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Ok(())
    })?;
    Ok(AlterTable {
        alter_span,
        online,
        ignore,
        table_span,
        if_exists,
        table,
        alter_specifications,
    })
}

pub(crate) fn parse_alter<'a>(parser: &mut Parser<'a>) -> Result<Statement<'a>, ParseError> {
    let alter_span = parser.consume_keyword(Keyword::ALTER)?;

    let online = parser.skip_keyword(Keyword::ONLINE);
    let ignore = parser.skip_keyword(Keyword::IGNORE);

    match &parser.token {
        Token::Ident(_, Keyword::TABLE) => Ok(Statement::AlterTable(parse_alter_table(
            parser, alter_span, online, ignore,
        )?)),
        _ => parser.expected_failure("alterable"),
    }
}
