use std::borrow::Cow;

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
    data_type::parse_data_type,
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    DataType, Span, Spanned, Statement,
};

#[derive(Clone, Debug)]
pub enum IndexOption<'a> {
    IndexTypeBTree(Span),
    IndexTypeHash(Span),
    IndexTypeRTree(Span),
    Comment((Cow<'a, str>, Span)),
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
pub enum ForeginKeyOnType {
    Update(Span),
    Delete(Span),
}

#[derive(Clone, Debug)]
pub enum ForeginKeyOnAction {
    Restrict(Span),
    Cascade(Span),
    SetNull(Span),
    NoAction(Span),
    SetDefault(Span),
}

#[derive(Clone, Debug)]
pub struct ForeginKeyOn {
    pub type_: ForeginKeyOnType,
    pub action: ForeginKeyOnAction,
}

#[derive(Clone, Debug)]
pub struct IndexCol<'a> {
    pub name: (&'a str, Span),
    pub size: Option<(u32, Span)>,
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
        cols: Vec<IndexCol<'a>>,
        index_options: Vec<IndexOption<'a>>,
    },
    AddForeginKey {
        add_span: Span,
        constraint: Option<(Span, Option<(&'a str, Span)>)>,
        foregin_key_span: Span,
        if_not_exists: Option<Span>,
        name: Option<(&'a str, Span)>,
        cols: Vec<IndexCol<'a>>,
        references_span: Span,
        references_table: (&'a str, Span),
        references_cols: Vec<(&'a str, Span)>,
        ons: Vec<ForeginKeyOn>,
    },
    Modify {
        modify_span: Span,
        if_exists: Option<Span>,
        col: (&'a str, Span),
        definition: DataType<'a>,
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
            Token::Ident(_, Keyword::COMMENT) => {
                parser.consume_keyword(Keyword::COMMENT)?;
                out.push(IndexOption::Comment(parser.consume_string()?))
            }
            _ => break,
        }
    }
    Ok(())
}

fn parse_index_cols<'a>(parser: &mut Parser<'a>) -> Result<Vec<IndexCol<'a>>, ParseError> {
    parser.consume_token(Token::LParen)?;
    let mut ans = Vec::new();
    parser.recovered("')'", &|t| t == &Token::RParen, |parser| {
        loop {
            let name = parser.consume_plain_identifier()?;
            let size = if parser.skip_token(Token::LParen).is_some() {
                let size = parser.recovered("')'", &|t| t == &Token::RParen, |parser| {
                    parser.consume_int()
                })?;
                parser.consume_token(Token::RParen)?;
                Some(size)
            } else {
                None
            };

            // TODO [ASC | DESC]

            ans.push(IndexCol { name, size });
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Ok(())
    })?;
    parser.consume_token(Token::RParen)?;
    Ok(ans)
}

fn parse_cols<'a>(parser: &mut Parser<'a>) -> Result<Vec<(&'a str, Span)>, ParseError> {
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
        Token::Ident(_, Keyword::FOREIGN) => {
            let foregin_key_span = parser
                .consume_keyword(Keyword::FOREIGN)?
                .join_span(&parser.consume_keyword(Keyword::KEY)?);
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

            let cols = parse_index_cols(parser)?;
            let references_span = parser.consume_keyword(Keyword::REFERENCES)?;
            let references_table = parser.consume_plain_identifier()?;
            let references_cols = parse_cols(parser)?;
            let mut ons = Vec::new();
            while let Some(on) = parser.skip_keyword(Keyword::ON) {
                let type_ = match parser.token {
                    Token::Ident(_, Keyword::UPDATE) => ForeginKeyOnType::Update(
                        parser.consume_keyword(Keyword::UPDATE)?.join_span(&on),
                    ),
                    Token::Ident(_, Keyword::DELETE) => ForeginKeyOnType::Delete(
                        parser.consume_keyword(Keyword::DELETE)?.join_span(&on),
                    ),
                    _ => parser.expected_failure("'UPDATE' or 'DELETE'")?,
                };

                let action = match parser.token {
                    Token::Ident(_, Keyword::RESTRICT) => {
                        ForeginKeyOnAction::Restrict(parser.consume_keyword(Keyword::RESTRICT)?)
                    }
                    Token::Ident(_, Keyword::CASCADE) => {
                        ForeginKeyOnAction::Cascade(parser.consume_keyword(Keyword::CASCADE)?)
                    }
                    Token::Ident(_, Keyword::SET) => {
                        let set = parser.consume_keyword(Keyword::SET)?;
                        match parser.token {
                            Token::Ident(_, Keyword::NULL) => ForeginKeyOnAction::SetNull(
                                parser.consume_keyword(Keyword::NULL)?.join_span(&set),
                            ),
                            Token::Ident(_, Keyword::DELETE) => ForeginKeyOnAction::SetDefault(
                                parser.consume_keyword(Keyword::DEFAULT)?.join_span(&set),
                            ),
                            _ => parser.expected_failure("'NULL' or 'DEFAULT'")?,
                        }
                    }
                    Token::Ident(_, Keyword::NO) => ForeginKeyOnAction::SetNull(
                        parser
                            .consume_keyword(Keyword::NO)?
                            .join_span(&parser.consume_keyword(Keyword::ACTION)?),
                    ),
                    _ => parser.expected_failure("'RESTRICT' or 'CASCADE', 'SET' or 'NO")?,
                };
                ons.push(ForeginKeyOn { type_, action })
            }
            Ok(AlterSpecification::AddForeginKey {
                add_span,
                constraint,
                foregin_key_span,
                if_not_exists,
                name,
                cols,
                references_span,
                references_table,
                references_cols,
                ons,
            })
        }
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
                Token::Ident(_, Keyword::MODIFY) => {
                    let mut modify_span = parser.consume_keyword(Keyword::MODIFY)?;
                    if let Some(v) = parser.skip_keyword(Keyword::COLUMN) {
                        modify_span = modify_span.join_span(&v);
                    }
                    let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                        Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
                    } else {
                        None
                    };
                    let col = parser.consume_plain_identifier()?;
                    let definition = parse_data_type(parser)?;
                    // TODO [FIRST | AFTER col_name]
                    AlterSpecification::Modify {
                        modify_span,
                        if_exists,
                        col,
                        definition,
                    }
                }
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
