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
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    Identifier, Span, Spanned, Statement,
};

#[derive(Debug, Clone)]
pub struct DropTable<'a> {
    pub drop_span: Span,
    pub temporary: Option<Span>,
    pub table_span: Span,
    pub if_exists: Option<Span>,
    pub tables: Vec<Identifier<'a>>,
}

impl<'a> Spanned for DropTable<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.temporary)
            .join_span(&self.table_span)
            .join_span(&self.if_exists)
            .join_span(&self.tables)
    }
}

#[derive(Debug, Clone)]
pub struct DropView<'a> {
    pub drop_span: Span,
    pub temporary: Option<Span>,
    pub view_span: Span,
    pub if_exists: Option<Span>,
    pub views: Vec<Identifier<'a>>,
}

impl<'a> Spanned for DropView<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.temporary)
            .join_span(&self.view_span)
            .join_span(&self.if_exists)
            .join_span(&self.views)
    }
}

#[derive(Debug, Clone)]
pub struct DropDatabase<'a> {
    pub drop_span: Span,
    pub database_span: Span,
    pub if_exists: Option<Span>,
    pub database: Identifier<'a>,
}

impl<'a> Spanned for DropDatabase<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.database_span)
            .join_span(&self.if_exists)
            .join_span(&self.database)
    }
}

#[derive(Debug, Clone)]
pub struct DropEvent<'a> {
    pub drop_span: Span,
    pub event_span: Span,
    pub if_exists: Option<Span>,
    pub event: Identifier<'a>,
}

impl<'a> Spanned for DropEvent<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.event_span)
            .join_span(&self.if_exists)
            .join_span(&self.event)
    }
}

#[derive(Debug, Clone)]
pub struct DropFunction<'a> {
    pub drop_span: Span,
    pub function_span: Span,
    pub if_exists: Option<Span>,
    pub function: Identifier<'a>,
}

impl<'a> Spanned for DropFunction<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.function_span)
            .join_span(&self.if_exists)
            .join_span(&self.function)
    }
}

#[derive(Debug, Clone)]
pub struct DropProcedure<'a> {
    pub drop_span: Span,
    pub procedure_span: Span,
    pub if_exists: Option<Span>,
    pub procedure: Identifier<'a>,
}

impl<'a> Spanned for DropProcedure<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.procedure_span)
            .join_span(&self.if_exists)
            .join_span(&self.procedure)
    }
}

#[derive(Debug, Clone)]
pub struct DropServer<'a> {
    pub drop_span: Span,
    pub server_span: Span,
    pub if_exists: Option<Span>,
    pub server: Identifier<'a>,
}

impl<'a> Spanned for DropServer<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.server_span)
            .join_span(&self.if_exists)
            .join_span(&self.server)
    }
}

#[derive(Debug, Clone)]
pub struct DropTrigger<'a> {
    pub drop_span: Span,
    pub trigger_span: Span,
    pub if_exists: Option<Span>,
    pub schema: Option<Identifier<'a>>,
    pub trigger: Identifier<'a>,
}

impl<'a> Spanned for DropTrigger<'a> {
    fn span(&self) -> Span {
        self.drop_span
            .join_span(&self.trigger_span)
            .join_span(&self.if_exists)
            .join_span(&self.schema)
            .join_span(&self.trigger)
    }
}

pub(crate) fn parse_drop<'a, 'b>(parser: &mut Parser<'a, 'b>) -> Result<Statement<'a>, ParseError> {
    let drop_span = parser.consume_keyword(Keyword::DROP)?;
    let temporary = parser.skip_keyword(Keyword::TEMPORARY);
    match &parser.token {
        Token::Ident(_, Keyword::TABLE) => {
            let table_span = parser.consume_keyword(Keyword::TABLE)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let mut tables = Vec::new();
            loop {
                tables.push(parser.consume_plain_identifier()?);
                if parser.skip_token(Token::Comma).is_none() {
                    break;
                }
            }
            Ok(Statement::DropTable(DropTable {
                drop_span,
                temporary,
                table_span,
                if_exists,
                tables,
            }))
        }
        Token::Ident(_, kw @ Keyword::DATABASE | kw @ Keyword::SCHEMA) => {
            // TODO complain about temporary
            let kw = *kw;
            let database_span = parser.consume_keyword(kw)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let database = parser.consume_plain_identifier()?;
            Ok(Statement::DropDatabase(DropDatabase {
                drop_span,
                database_span,
                if_exists,
                database,
            }))
        }
        Token::Ident(_, Keyword::EVENT) => {
            // TODO complain about temporary
            let event_span = parser.consume_keyword(Keyword::EVENT)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let event = parser.consume_plain_identifier()?;
            Ok(Statement::DropEvent(DropEvent {
                drop_span,
                event_span,
                if_exists,
                event,
            }))
        }
        Token::Ident(_, Keyword::FUNCTION) => {
            // TODO complain about temporary
            let function_span = parser.consume_keyword(Keyword::FUNCTION)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let function = parser.consume_plain_identifier()?;
            Ok(Statement::DropFunction(DropFunction {
                drop_span,
                function_span,
                if_exists,
                function,
            }))
        }
        Token::Ident(_, Keyword::INDEX) => {
            // DROP INDEX [IF EXISTS] index_name ON tbl_name
            parser.todo(file!(), line!())
        }
        Token::Ident(_, Keyword::PROCEDURE) => {
            // TODO complain about temporary
            let procedure_span = parser.consume_keyword(Keyword::PROCEDURE)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let procedure = parser.consume_plain_identifier()?;
            Ok(Statement::DropProcedure(DropProcedure {
                drop_span,
                procedure_span,
                if_exists,
                procedure,
            }))
        }
        Token::Ident(_, Keyword::SEQUENCE) => {
            // DROP [TEMPORARY] SEQUENCE [IF EXISTS] [/*COMMENT TO SAVE*/] sequence_name [, sequence_name] ...
            parser.todo(file!(), line!())
        }
        Token::Ident(_, Keyword::SERVER) => {
            // TODO complain about temporary
            let server_span = parser.consume_keyword(Keyword::SERVER)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let server = parser.consume_plain_identifier()?;
            Ok(Statement::DropServer(DropServer {
                drop_span,
                server_span,
                if_exists,
                server,
            }))
        }
        Token::Ident(_, Keyword::TRIGGER) => {
            let trigger_span = parser.consume_keyword(Keyword::TRIGGER)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let n1 = parser.consume_plain_identifier()?;
            let (schema, trigger) = if parser.skip_token(Token::Comma).is_some() {
                (Some(n1), parser.consume_plain_identifier()?)
            } else {
                (None, n1)
            };
            Ok(Statement::DropTrigger(DropTrigger {
                drop_span,
                trigger_span,
                if_exists,
                schema,
                trigger,
            }))
        }
        Token::Ident(_, Keyword::VIEW) => {
            let view_span = parser.consume_keyword(Keyword::VIEW)?;
            let if_exists = if let Some(span) = parser.skip_keyword(Keyword::IF) {
                Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&span))
            } else {
                None
            };
            let mut views = Vec::new();
            loop {
                views.push(parser.consume_plain_identifier()?);
                if parser.skip_token(Token::Comma).is_none() {
                    break;
                }
            }
            // TODO  [RESTRICT | CASCADE]
            Ok(Statement::DropView(DropView {
                drop_span,
                temporary,
                view_span,
                if_exists,
                views,
            }))
        }
        Token::Ident(_, Keyword::USER) => {
            // DROP USER [IF EXISTS] user_name [, user_name] ..
            parser.todo(file!(), line!())
        }
        _ => parser.expected_failure("droppable"),
    }
}
