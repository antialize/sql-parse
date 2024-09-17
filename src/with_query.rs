use alloc::{boxed::Box, vec::Vec};

use crate::{
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    statement::parse_statement,
    Identifier, Span, Spanned, Statement,
};

#[derive(Clone, Debug)]
pub struct WithBlock<'a> {
    // Identifier for the with block
    pub identifier: Identifier<'a>,
    // Span of AS
    pub as_span: Span,
    // Span of (
    pub lparen_span: Span,
    // The statement within the with block, will be one of select, update, insert or delete
    pub statement: Statement<'a>,
    // Span of )
    pub rparen_span: Span,
}

impl<'a> Spanned for WithBlock<'a> {
    fn span(&self) -> Span {
        self.identifier
            .span()
            .join_span(&self.as_span)
            .join_span(&self.lparen_span)
            .join_span(&self.statement)
            .join_span(&self.rparen_span)
    }
}

/// Represent a with query statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, WithQuery, Statement, Issues};
/// # let options = ParseOptions::new().dialect(SQLDialect::PostgreSQL);
/// #
/// let sql = "WITH ids AS (DELETE FROM things WHERE number=42) INSERT INTO deleted (id) SELECT id FROM ids;";
/// let mut issues = Issues::new(sql);
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_ok());
/// # 
/// let delete: WithQuery = match stmts.pop() {
///     Some(Statement::WithQuery(d)) => d,
///     _ => panic!("We should get a with statement")
/// };
///
/// assert!(delete.with_blocks[0].identifier.as_str() == "ids");
/// ```
#[derive(Clone, Debug)]
pub struct WithQuery<'a> {
    // Span of WITH
    pub with_span: Span,
    // The comma seperated with blocks
    pub with_blocks: Vec<WithBlock<'a>>,
    // The final statement of the with query, will be one of select, update, insert, delete or merge
    pub statement: Box<Statement<'a>>,
}

impl<'a> Spanned for WithQuery<'a> {
    fn span(&self) -> Span {
        self.with_span
            .join_span(&self.with_blocks)
            .join_span(&self.statement)
    }
}

pub(crate) fn parse_with_query<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<WithQuery<'a>, ParseError> {
    let with_span = parser.consume_keyword(Keyword::WITH)?;
    let mut with_blocks = Vec::new();
    loop {
        let identifier = parser.consume_plain_identifier()?;
        let as_span = parser.consume_keyword(Keyword::AS)?;
        let lparen_span = parser.consume_token(Token::LParen)?;
        let statement =
            parser.recovered(
                "')'",
                &|t| t == &Token::RParen,
                |parser| match parse_statement(parser)? {
                    Some(v) => Ok(Some(v)),
                    None => {
                        parser.expected_error("Statement");
                        Ok(None)
                    }
                },
            )?;
        let rparen_span = parser.consume_token(Token::RParen)?;
        let statement = match statement {
            Some(v) => {
                if !matches!(
                    &v,
                    Statement::Select(_)
                        | Statement::InsertReplace(_)
                        | Statement::Update(_)
                        | Statement::Delete(_)
                ) {
                    parser.err(
                        "Only SELECT, INSERT, UPDATE or DELETE allowed within WITH query",
                        &v.span(),
                    );
                }
                v
            }
            None => Statement::Begin(lparen_span.clone()),
        };
        with_blocks.push(WithBlock {
            identifier,
            as_span,
            lparen_span,
            statement,
            rparen_span,
        });
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }
    let statement = match parse_statement(parser)? {
        Some(v) => {
            // TODO merge statements are also allowed here
            if !matches!(
                &v,
                Statement::Select(_)
                    | Statement::InsertReplace(_)
                    | Statement::Update(_)
                    | Statement::Delete(_)
            ) {
                parser.err(
                    "Only SELECT, INSERT, UPDATE or DELETE allowed as WITH query",
                    &v.span(),
                );
            }
            Box::new(v)
        }
        None => parser.expected_failure("Statement")?,
    };
    let res = WithQuery {
        with_span,
        with_blocks,
        statement,
    };
    if !parser.options.dialect.is_postgresql() {
        parser.err("WITH statements only supported by postgresql", &res.span());
    }
    Ok(res)
}
