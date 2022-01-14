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
    alter::{parse_alter, AlterTable},
    create::{parse_create, CreateFunction, CreateTable, CreateTrigger, CreateView},
    delete::{parse_delete, Delete},
    drop::{
        parse_drop, DropDatabase, DropEvent, DropFunction, DropProcedure, DropServer, DropTable,
        DropTrigger, DropView,
    },
    expression::{parse_expression, Expression},
    insert::{parse_insert, Insert},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    select::{parse_select, Select},
    update::{parse_update, Update},
    Level, Span, Spanned,
};

#[derive(Clone, Debug)]
pub struct Set<'a> {
    pub set_span: Span,
    pub values: Vec<((&'a str, Span), Expression<'a>)>,
}

fn parse_set<'a>(parser: &mut Parser<'a>) -> Result<Set<'a>, ParseError> {
    let set_span = parser.consume_keyword(Keyword::SET)?;
    let mut values = Vec::new();
    loop {
        let name = parser.consume_plain_identifier()?;
        parser.consume_token(Token::Eq)?;
        let val = parse_expression(parser, false)?;
        values.push((name, val));
        if parser.skip_token(Token::Comma).is_none() {
            break;
        }
    }
    Ok(Set { set_span, values })
}

fn parse_statement_list_inner<'a>(
    parser: &mut Parser<'a>,
    out: &mut Vec<Statement<'a>>,
) -> Result<(), ParseError> {
    let old_delimiter = std::mem::replace(&mut parser.delimiter, Token::SemiColon);
    loop {
        while parser.skip_token(Token::SemiColon).is_some() {}
        match parse_statement(parser)? {
            Some(v) => out.push(v),
            None => break,
        }
        if parser.skip_token(Token::SemiColon).is_none() {
            break;
        }
    }
    Ok(())
}

fn parse_statement_list<'a>(
    parser: &mut Parser<'a>,
    out: &mut Vec<Statement<'a>>,
) -> Result<(), ParseError> {
    let old_delimiter = std::mem::replace(&mut parser.delimiter, Token::SemiColon);
    let r = parse_statement_list_inner(parser, out);
    parser.delimiter = old_delimiter;
    r
}

fn parse_block<'a>(parser: &mut Parser<'a>) -> Result<Vec<Statement<'a>>, ParseError> {
    parser.consume_keyword(Keyword::BEGIN)?;
    let mut ans = Vec::new();
    parser.recovered(
        "'END'",
        &|e| matches!(e, Token::Ident(_, Keyword::END)),
        |parser| parse_statement_list(parser, &mut ans),
    )?;
    parser.consume_keyword(Keyword::END)?;
    Ok(ans)
}

#[derive(Clone, Debug)]
pub struct IfCondition<'a> {
    pub elseif_span: Option<Span>,
    pub search_condition: Expression<'a>,
    pub then_span: Span,
    pub then: Vec<Statement<'a>>,
}

#[derive(Clone, Debug)]
pub struct If<'a> {
    pub if_span: Span,
    pub conditions: Vec<IfCondition<'a>>,
    pub else_: Option<(Span, Vec<Statement<'a>>)>,
    pub endif_span: Span,
}

fn parse_if<'a>(parser: &mut Parser<'a>) -> Result<If<'a>, ParseError> {
    let if_span = parser.consume_keyword(Keyword::IF)?;
    let mut conditions = Vec::new();
    let mut else_ = None;
    parser.recovered(
        "'END'",
        &|e| matches!(e, Token::Ident(_, Keyword::END)),
        |parser| {
            let search_condition = parse_expression(parser, false)?;
            let then_span = parser.consume_keyword(Keyword::THEN)?;
            let mut then = Vec::new();
            parse_statement_list(parser, &mut then)?;
            conditions.push(IfCondition {
                elseif_span: None,
                search_condition,
                then_span,
                then,
            });
            while let Some(elseif_span) = parser.skip_keyword(Keyword::ELSEIF) {
                let search_condition = parse_expression(parser, false)?;
                let then_span = parser.consume_keyword(Keyword::THEN)?;
                let mut then = Vec::new();
                parse_statement_list(parser, &mut then)?;
                conditions.push(IfCondition {
                    elseif_span: Some(elseif_span),
                    search_condition,
                    then_span,
                    then,
                })
            }
            if let Some(else_span) = parser.skip_keyword(Keyword::ELSE) {
                let mut o = Vec::new();
                parse_statement_list(parser, &mut o)?;
                else_ = Some((else_span, o));
            }
            Ok(())
        },
    )?;
    let endif_span = parser.consume_keywords(&[Keyword::END, Keyword::IF])?;
    Ok(If {
        if_span,
        conditions,
        else_,
        endif_span,
    })
}

#[derive(Clone, Debug)]
pub enum Statement<'a> {
    CreateTable(CreateTable<'a>),
    CreateView(CreateView<'a>),
    CreateTrigger(CreateTrigger<'a>),
    CreateFunction(CreateFunction<'a>),
    Select(Select<'a>),
    Delete(Delete<'a>),
    Insert(Insert<'a>),
    Update(Update<'a>),
    DropTable(DropTable<'a>),
    DropFunction(DropFunction<'a>),
    DropProcedure(DropProcedure<'a>),
    DropEvent(DropEvent<'a>),
    DropDatabase(DropDatabase<'a>),
    DropServer(DropServer<'a>),
    DropTrigger(DropTrigger<'a>),
    DropView(DropView<'a>),
    Set(Set<'a>),
    AlterTable(AlterTable<'a>),
    Block(Vec<Statement<'a>>),
    If(If<'a>),
}

pub(crate) fn parse_statement<'a>(
    parser: &mut Parser<'a>,
) -> Result<Option<Statement<'a>>, ParseError> {
    Ok(match &parser.token {
        Token::Ident(_, Keyword::CREATE) => Some(parse_create(parser)?),
        Token::Ident(_, Keyword::DROP) => Some(parse_drop(parser)?),
        Token::Ident(_, Keyword::SELECT) => Some(Statement::Select(parse_select(parser)?)),
        Token::Ident(_, Keyword::DELETE) => Some(Statement::Delete(parse_delete(parser)?)),
        Token::Ident(_, Keyword::INSERT) => Some(Statement::Insert(parse_insert(parser)?)),
        Token::Ident(_, Keyword::UPDATE) => Some(Statement::Update(parse_update(parser)?)),
        Token::Ident(_, Keyword::SET) => Some(Statement::Set(parse_set(parser)?)),
        Token::Ident(_, Keyword::BEGIN) => Some(Statement::Block(parse_block(parser)?)),
        Token::Ident(_, Keyword::IF) => Some(Statement::If(parse_if(parser)?)),
        Token::Ident(_, Keyword::ALTER) => Some(parse_alter(parser)?),
        _ => None,
    })
}

pub(crate) fn parse_statements<'a>(parser: &mut Parser<'a>) -> Vec<Statement<'a>> {
    let mut ans = Vec::new();
    loop {
        loop {
            match &parser.token {
                t if t == &parser.delimiter => {
                    parser.consume();
                }
                Token::Eof => return ans,
                _ => break,
            }
        }

        if parser.skip_keyword(Keyword::DELIMITER).is_some() {
            let t = parser.token.clone();

            if !matches!(t, Token::DoubleDollar | Token::SemiColon) {
                parser.issues.push(crate::Issue {
                    level: Level::Warning,
                    message: "Unknown delimiter".to_string(),
                    span: parser.span.clone(),
                    fragments: Vec::new(),
                });
            }
            parser.delimiter = t;
            parser.next();
            continue;
        }

        let stmt = match parse_statement(parser) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => parser.expected_failure("Statement"),
            Err(e) => Err(e),
        };
        let err = stmt.is_err();
        if let Ok(stmt) = stmt {
            ans.push(stmt);
        }

        if parser.token != parser.delimiter {
            if !err {
                parser.expected_error(parser.delimiter.name());
            }
            // We use a custom recovery here as ; is not allowed in sub expressions, it always terminates outer most statements
            loop {
                parser.next();
                match &parser.token {
                    t if t == &parser.delimiter => break,
                    Token::Eof => return ans,
                    _ => (),
                }
            }
        }
        parser.consume_token(parser.delimiter.clone()).unwrap();
    }
}
