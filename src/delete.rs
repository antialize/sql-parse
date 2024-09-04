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

use alloc::vec::Vec;

use crate::{
    expression::{parse_expression, Expression},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    qualified_name::parse_qualified_name,
    select::{parse_select_expr, parse_table_reference},
    Issue, QualifiedName, SelectExpr, Span, Spanned, TableReference,
};

/// Flags for deletion
#[derive(Clone, Debug)]
pub enum DeleteFlag {
    LowPriority(Span),
    Quick(Span),
    Ignore(Span),
}

impl Spanned for DeleteFlag {
    fn span(&self) -> Span {
        match &self {
            DeleteFlag::LowPriority(v) => v.span(),
            DeleteFlag::Quick(v) => v.span(),
            DeleteFlag::Ignore(v) => v.span(),
        }
    }
}

/// Represent a delete statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, Delete, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "DELETE FROM t1 WHERE c1 IN (SELECT b.c1 FROM t1 b WHERE b.c2=0);";
///
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let delete: Delete = match stmts.pop() {
///     Some(Statement::Delete(d)) => d,
///     _ => panic!("We should get a delete statement")
/// };
///
/// assert!(delete.tables[0].identifier.as_str() == "t1");
/// println!("{:#?}", delete.where_);
///
/// let sql = "DELETE `t1` FROM `t1` LEFT JOIN `t2` ON `t1`.`t2_id`=`t2`.`id` WHERE `t2`.`key`='my_key';";
///
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// ```
#[derive(Clone, Debug)]
pub struct Delete<'a> {
    /// Span of "DELETE"
    pub delete_span: Span,
    /// Flags following "DELETE"
    pub flags: Vec<DeleteFlag>,
    /// Span of "FROM"
    pub from_span: Span,
    /// Tables to do deletes on
    pub tables: Vec<QualifiedName<'a>>,
    /// Table to use in where clause in multi table delete
    pub using: Vec<TableReference<'a>>,
    /// Where expression and Span of "WHERE" if specified
    pub where_: Option<(Expression<'a>, Span)>,
    /// Span of "RETURNING" and select expressions after "RETURNING", if "RETURNING" is present
    pub returning: Option<(Span, Vec<SelectExpr<'a>>)>,
}

impl<'a> Spanned for Delete<'a> {
    fn span(&self) -> Span {
        self.delete_span
            .join_span(&self.flags)
            .join_span(&self.from_span)
            .join_span(&self.tables)
            .join_span(&self.using)
            .join_span(&self.where_)
            .join_span(&self.returning)
    }
}

pub(crate) fn parse_delete<'a>(parser: &mut Parser<'a, '_>) -> Result<Delete<'a>, ParseError> {
    let delete_span = parser.consume_keyword(Keyword::DELETE)?;
    let mut flags = Vec::new();

    loop {
        match &parser.token {
            Token::Ident(_, Keyword::LOW_PRIORITY) => flags.push(DeleteFlag::LowPriority(
                parser.consume_keyword(Keyword::LOW_PRIORITY)?,
            )),
            Token::Ident(_, Keyword::QUICK) => {
                flags.push(DeleteFlag::Quick(parser.consume_keyword(Keyword::QUICK)?))
            }
            Token::Ident(_, Keyword::IGNORE) => {
                flags.push(DeleteFlag::Ignore(parser.consume_keyword(Keyword::IGNORE)?))
            }
            _ => break,
        }
    }

    let mut tables = Vec::new();
    let mut using = Vec::new();
    let from_span = if let Some(from_span) = parser.skip_keyword(Keyword::FROM) {
        loop {
            tables.push(parse_qualified_name(parser)?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        from_span
    } else {
        loop {
            tables.push(parse_qualified_name(parser)?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        let from_span = parser.consume_keyword(Keyword::FROM)?;
        loop {
            using.push(parse_table_reference(parser)?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        from_span
    };

    //TODO [PARTITION (partition_list)]
    //TODO [FOR PORTION OF period FROM expr1 TO expr2]

    if let Some(using_span) = parser.skip_keyword(Keyword::USING) {
        if !using.is_empty() {
            parser.add_error(
                "Using not allowed in delete with table names before FROM",
                &using_span,
            );
        }
        loop {
            using.push(parse_table_reference(parser)?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
    }

    let where_ = if let Some(span) = parser.skip_keyword(Keyword::WHERE) {
        Some((parse_expression(parser, false)?, span))
    } else {
        None
    };
    //TODO [ORDER BY ...]
    //TODO LIMIT row_count]

    let returning = if let Some(returning_span) = parser.skip_keyword(Keyword::RETURNING) {
        let mut returning_exprs = Vec::new();
        loop {
            returning_exprs.push(parse_select_expr(parser)?);
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Some((returning_span, returning_exprs))
    } else {
        None
    };

    Ok(Delete {
        flags,
        delete_span,
        tables,
        using,
        from_span,
        where_,
        returning,
    })
}
