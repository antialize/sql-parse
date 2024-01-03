use alloc::vec::Vec;

use crate::{
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    qualified_name::parse_qualified_name,
    QualifiedName, Span, Spanned,
};

#[derive(Debug, Clone)]
pub struct TableToTable<'a> {
    pub table: QualifiedName<'a>,
    /// Span of "TO"
    pub to_span: Span,
    pub new_table: QualifiedName<'a>,
}

impl<'a> Spanned for TableToTable<'a> {
    fn span(&self) -> Span {
        self.table
            .join_span(&self.to_span)
            .join_span(&self.new_table)
    }
}

/// Represent a rename table statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, RenameTable, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "RENAME TABLE `t1` TO `t2`;";
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty(), "Issues: {:#?}", issues);
/// #
/// let rename_table: RenameTable = match stmts.pop() {
///     Some(Statement::RenameTable(c)) => c,
///     _ => panic!("We should get a rename table statement")
/// };
///
/// assert!(rename_table.table_to_tables.get(0).unwrap().table.identifier.as_str() == "t1");
///
///

#[derive(Debug, Clone)]
pub struct RenameTable<'a> {
    /// Span of "RENAME"
    pub rename_span: Span,
    /// Span of "TABLE" if specified
    pub table_span: Option<Span>,
    pub table_to_tables: Vec<TableToTable<'a>>,
}

impl<'a> Spanned for RenameTable<'a> {
    fn span(&self) -> Span {
        self.rename_span
            .join_span(&self.table_span)
            .join_span(&self.table_to_tables)
    }
}

pub(crate) fn parse_rename_table<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<RenameTable<'a>, ParseError> {
    let rename_span = parser.consume_keyword(Keyword::RENAME)?;
    let table_span = parser.skip_keyword(Keyword::TABLE);
    let mut table_to_tables = Vec::new();
    loop {
        let table = parse_qualified_name(parser)?;
        let to_span = parser.consume_keyword(Keyword::TO)?;
        let new_table = parse_qualified_name(parser)?;
        table_to_tables.push(TableToTable {
            table,
            to_span,
            new_table,
        });
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }
    Ok(RenameTable {
        rename_span,
        table_span,
        table_to_tables,
    })
}
