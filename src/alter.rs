use alloc::vec::Vec;

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
    DataType, Identifier, SString, Span, Spanned, Statement,
};

/// Option on an index
#[derive(Clone, Debug)]
pub enum IndexOption<'a> {
    /// The index should be a BTree
    IndexTypeBTree(Span),
    /// The index should be hashed
    IndexTypeHash(Span),
    /// The index should be an RTree
    IndexTypeRTree(Span),
    /// Attach a comment to the index
    Comment(SString<'a>),
}

impl<'a> Spanned for IndexOption<'a> {
    fn span(&self) -> Span {
        match &self {
            IndexOption::IndexTypeBTree(v) => v.span(),
            IndexOption::IndexTypeHash(v) => v.span(),
            IndexOption::IndexTypeRTree(v) => v.span(),
            IndexOption::Comment(v) => v.span(),
        }
    }
}

/// Type of index to add
#[derive(Clone, Debug)]
pub enum IndexType {
    Index(Span),
    Primary(Span),
    Unique(Span),
    FullText(Span),
    Spatial(Span),
}

impl Spanned for IndexType {
    fn span(&self) -> Span {
        match &self {
            IndexType::Index(v) => v.span(),
            IndexType::Primary(v) => v.span(),
            IndexType::Unique(v) => v.span(),
            IndexType::FullText(v) => v.span(),
            IndexType::Spatial(v) => v.span(),
        }
    }
}

/// When to take a foreign key action
#[derive(Clone, Debug)]
pub enum ForeignKeyOnType {
    Update(Span),
    Delete(Span),
}

impl Spanned for ForeignKeyOnType {
    fn span(&self) -> Span {
        match &self {
            ForeignKeyOnType::Update(v) => v.span(),
            ForeignKeyOnType::Delete(v) => v.span(),
        }
    }
}

/// Action to take on event for foreign key
#[derive(Clone, Debug)]
pub enum ForeignKeyOnAction {
    Restrict(Span),
    Cascade(Span),
    SetNull(Span),
    NoAction(Span),
    SetDefault(Span),
}

impl Spanned for ForeignKeyOnAction {
    fn span(&self) -> Span {
        match &self {
            ForeignKeyOnAction::Restrict(v) => v.span(),
            ForeignKeyOnAction::Cascade(v) => v.span(),
            ForeignKeyOnAction::SetNull(v) => v.span(),
            ForeignKeyOnAction::NoAction(v) => v.span(),
            ForeignKeyOnAction::SetDefault(v) => v.span(),
        }
    }
}

/// Action to perform on events on foreign keys
#[derive(Clone, Debug)]
pub struct ForeignKeyOn {
    pub type_: ForeignKeyOnType,
    pub action: ForeignKeyOnAction,
}

impl Spanned for ForeignKeyOn {
    fn span(&self) -> Span {
        self.type_.join_span(&self.action)
    }
}

/// Specify a column for an index, together with a with
#[derive(Clone, Debug)]
pub struct IndexCol<'a> {
    /// The name of the column
    pub name: Identifier<'a>,
    /// Optional width of index together with its span
    pub size: Option<(u32, Span)>,
}

impl<'a> Spanned for IndexCol<'a> {
    fn span(&self) -> Span {
        self.name.join_span(&self.size)
    }
}

/// Enum of alterations to perform on a table
#[derive(Clone, Debug)]
pub enum AlterSpecification<'a> {
    AddColumn {
        add_span: Span,
        identifier: Identifier<'a>,
        data_type: DataType<'a>,
    },
    /// Add an index
    AddIndex {
        /// Span of "ADD"
        add_span: Span,
        /// The type of index to add
        index_type: IndexType,
        /// Span of "IF NOT EXISTS" if specified
        if_not_exists: Option<Span>,
        /// Named of index if specified
        name: Option<Identifier<'a>>,
        /// Optional "CONSTRAINT" with symbol if specified
        constraint: Option<(Span, Option<Identifier<'a>>)>,
        /// Columns to add the index over
        cols: Vec<IndexCol<'a>>,
        /// Options on the index
        index_options: Vec<IndexOption<'a>>,
    },
    /// Add a foreign key
    AddForeignKey {
        /// Span of "ADD"
        add_span: Span,
        /// Optional "CONSTRAINT" with symbol if specified
        constraint: Option<(Span, Option<Identifier<'a>>)>,
        /// Span of "FOREIGN KEY"
        foreign_key_span: Span,
        /// Span of "IF NOT EXISTS" if specified
        if_not_exists: Option<Span>,
        /// Named of index if specified
        name: Option<Identifier<'a>>,
        /// Columns to add the index over
        cols: Vec<IndexCol<'a>>,
        /// Span of "REFERENCES"
        references_span: Span,
        /// Refereed table
        references_table: Identifier<'a>,
        /// Columns in referred table
        references_cols: Vec<Identifier<'a>>,
        /// List of what should happen at specified events
        ons: Vec<ForeignKeyOn>,
    },
    /// Modify a column
    Modify {
        // Span of "MODIFY"
        modify_span: Span,
        /// Span of "IF EXISTS" if specified
        if_exists: Option<Span>,
        /// Name of column to modify
        col: Identifier<'a>,
        /// New definition of column
        definition: DataType<'a>,
    },
    /// Modify a column
    OwnerTo {
        // Span of "OWNER TO"
        span: Span,
        /// Name of owner
        owner: Identifier<'a>,
    },
}

impl<'a> Spanned for AlterSpecification<'a> {
    fn span(&self) -> Span {
        match &self {
            AlterSpecification::AddColumn {
                add_span,
                identifier,
                data_type,
            } => add_span.join_span(identifier).join_span(data_type),
            AlterSpecification::AddIndex {
                add_span,
                index_type,
                if_not_exists,
                name,
                constraint,
                cols,
                index_options,
            } => add_span
                .join_span(index_type)
                .join_span(if_not_exists)
                .join_span(name)
                .join_span(constraint)
                .join_span(cols)
                .join_span(index_options),
            AlterSpecification::AddForeignKey {
                add_span,
                constraint,
                foreign_key_span: foregin_key_span,
                if_not_exists,
                name,
                cols,
                references_span,
                references_table,
                references_cols,
                ons,
            } => add_span
                .join_span(constraint)
                .join_span(foregin_key_span)
                .join_span(if_not_exists)
                .join_span(name)
                .join_span(cols)
                .join_span(references_span)
                .join_span(references_table)
                .join_span(references_cols)
                .join_span(ons),
            AlterSpecification::Modify {
                modify_span,
                if_exists,
                col,
                definition,
            } => modify_span
                .join_span(if_exists)
                .join_span(col)
                .join_span(definition),
            AlterSpecification::OwnerTo { span, owner } => span.join_span(owner),
        }
    }
}

fn parse_index_type<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
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

fn parse_index_options<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
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

fn parse_index_cols<'a, 'b>(parser: &mut Parser<'a, 'b>) -> Result<Vec<IndexCol<'a>>, ParseError> {
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

fn parse_cols<'a, 'b>(parser: &mut Parser<'a, 'b>) -> Result<Vec<Identifier<'a>>, ParseError> {
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

fn parse_add_alter_specification<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
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
            let foregin_key_span = parser.consume_keywords(&[Keyword::FOREIGN, Keyword::KEY])?;
            let if_not_exists = if let Some(s) = parser.skip_keyword(Keyword::IF) {
                Some(
                    parser
                        .consume_keywords(&[Keyword::NOT, Keyword::EXISTS])?
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
                    Token::Ident(_, Keyword::UPDATE) => ForeignKeyOnType::Update(
                        parser.consume_keyword(Keyword::UPDATE)?.join_span(&on),
                    ),
                    Token::Ident(_, Keyword::DELETE) => ForeignKeyOnType::Delete(
                        parser.consume_keyword(Keyword::DELETE)?.join_span(&on),
                    ),
                    _ => parser.expected_failure("'UPDATE' or 'DELETE'")?,
                };

                let action = match parser.token {
                    Token::Ident(_, Keyword::RESTRICT) => {
                        ForeignKeyOnAction::Restrict(parser.consume_keyword(Keyword::RESTRICT)?)
                    }
                    Token::Ident(_, Keyword::CASCADE) => {
                        ForeignKeyOnAction::Cascade(parser.consume_keyword(Keyword::CASCADE)?)
                    }
                    Token::Ident(_, Keyword::SET) => {
                        let set = parser.consume_keyword(Keyword::SET)?;
                        match parser.token {
                            Token::Ident(_, Keyword::NULL) => ForeignKeyOnAction::SetNull(
                                parser.consume_keyword(Keyword::NULL)?.join_span(&set),
                            ),
                            Token::Ident(_, Keyword::DELETE) => ForeignKeyOnAction::SetDefault(
                                parser.consume_keyword(Keyword::DEFAULT)?.join_span(&set),
                            ),
                            _ => parser.expected_failure("'NULL' or 'DEFAULT'")?,
                        }
                    }
                    Token::Ident(_, Keyword::NO) => ForeignKeyOnAction::SetNull(
                        parser.consume_keywords(&[Keyword::NO, Keyword::ACTION])?,
                    ),
                    _ => parser.expected_failure("'RESTRICT' or 'CASCADE', 'SET' or 'NO")?,
                };
                ons.push(ForeignKeyOn { type_, action })
            }
            Ok(AlterSpecification::AddForeignKey {
                add_span,
                constraint,
                foreign_key_span: foregin_key_span,
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
                Token::Ident(_, Keyword::PRIMARY) => {
                    IndexType::Primary(parser.consume_keywords(&[Keyword::PRIMARY, Keyword::KEY])?)
                }
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
                _ => parser.ice(file!(), line!())?,
            };

            let if_not_exists = if let Some(s) = parser.skip_keyword(Keyword::IF) {
                Some(
                    parser
                        .consume_keywords(&[Keyword::NOT, Keyword::EXISTS])?
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
        Token::Ident(_, Keyword::COLUMN) => {
            parser.consume_keyword(Keyword::COLUMN)?;
            let identifier = parser.consume_plain_identifier()?;
            let data_type = parse_data_type(parser, false)?;
            Ok(AlterSpecification::AddColumn {
                add_span,
                identifier,
                data_type,
            })
        }
        _ => parser.expected_failure("addable"),
    }
}

/// Represent an alter table statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, AlterTable, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "ALTER TABLE `t1`
///     MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,
///     ADD CONSTRAINT `t1_t2` FOREIGN KEY (`two`) REFERENCES `t2` (`id`);";
///
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let alter: AlterTable = match stmts.pop() {
///     Some(Statement::AlterTable(a)) => a,
///     _ => panic!("We should get an alter table statement")
/// };
///
/// assert!(alter.table.as_str() == "t1");
/// println!("{:#?}", alter.alter_specifications)
///
#[derive(Clone, Debug)]
pub struct AlterTable<'a> {
    /// Span of "ALTER"
    pub alter_span: Span,
    /// Span of "ONLINE" if specified
    pub online: Option<Span>,
    /// Span of "IGNORE" if specified
    pub ignore: Option<Span>,
    /// Span of "TABLE"
    pub table_span: Span,
    /// Span of "IF EXISTS" if specified
    pub if_exists: Option<Span>,
    /// The identifier of the table to alter
    pub table: Identifier<'a>,
    /// List of alterations to do
    pub alter_specifications: Vec<AlterSpecification<'a>>,
}

impl<'a> Spanned for AlterTable<'a> {
    fn span(&self) -> Span {
        self.alter_span
            .join_span(&self.online)
            .join_span(&self.ignore)
            .join_span(&self.table_span)
            .join_span(&self.if_exists)
            .join_span(&self.table)
            .join_span(&self.alter_specifications)
    }
}

fn parse_alter_table<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
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
                    let definition = parse_data_type(parser, false)?;
                    // TODO [FIRST | AFTER col_name]
                    AlterSpecification::Modify {
                        modify_span,
                        if_exists,
                        col,
                        definition,
                    }
                }
                Token::Ident(_, Keyword::OWNER) => {
                    let span = parser.consume_keywords(&[Keyword::OWNER, Keyword::TO])?;
                    let owner = parser.consume_plain_identifier()?;
                    AlterSpecification::OwnerTo { span, owner }
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

pub(crate) fn parse_alter<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<Statement<'a>, ParseError> {
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
