use crate::{
    keywords::Keyword,
    parser::{ParseError, Parser},
    qualified_name::parse_qualified_name,
    QualifiedName, Span, Spanned, Statement,
};

/// Represent a truncate table statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, TruncateTable, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "TRUNCATE TABLE `t1`;";
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty(), "Issues: {:#?}", issues);
/// #
/// let truncate_table: TruncateTable = match stmts.pop() {
///     Some(Statement::TruncateTable(c)) => c,
///     _ => panic!("We should get a truncate table statement")
/// };
///
/// assert!(truncate_table.table_name.identifier.as_str() == "t1");
///
/// ```

#[derive(Debug, Clone)]
pub struct TruncateTable<'a> {
    /// Span of "TRUNCATE"
    pub truncate_span: Span,
    /// Span of "TABLE" if specified
    pub table_span: Option<Span>,
    pub table_name: QualifiedName<'a>,
}

impl<'a> Spanned for TruncateTable<'a> {
    fn span(&self) -> Span {
        self.truncate_span
            .join_span(&self.table_span)
            .join_span(&self.table_name)
    }
}

pub(crate) fn parse_truncate_table<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<Statement<'a>, ParseError> {
    let truncate_span = parser.consume_keyword(Keyword::TRUNCATE)?;
    let table_span = parser.skip_keyword(Keyword::TABLE);
    let table_name = parse_qualified_name(parser)?;
    Ok(Statement::TruncateTable(TruncateTable {
        truncate_span,
        table_span,
        table_name,
    }))
}
