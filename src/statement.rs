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
    replace::{parse_replace, Replace},
    select::{parse_select, OrderFlag, Select},
    span::OptSpanned,
    update::{parse_update, Update},
    Level, Span, Spanned,
};

#[derive(Clone, Debug)]
pub struct Set<'a> {
    pub set_span: Span,
    pub values: Vec<((&'a str, Span), Expression<'a>)>,
}

impl<'a> Spanned for Set<'a> {
    fn span(&self) -> Span {
        self.set_span.join_span(&self.values)
    }
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

impl<'a> Spanned for IfCondition<'a> {
    fn span(&self) -> Span {
        self.then_span
            .join_span(&self.elseif_span)
            .join_span(&self.search_condition)
            .join_span(&self.then_span)
            .join_span(&self.then)
    }
}

#[derive(Clone, Debug)]
pub struct If<'a> {
    pub if_span: Span,
    pub conditions: Vec<IfCondition<'a>>,
    pub else_: Option<(Span, Vec<Statement<'a>>)>,
    pub endif_span: Span,
}

impl<'a> Spanned for If<'a> {
    fn span(&self) -> Span {
        self.if_span
            .join_span(&self.conditions)
            .join_span(&self.else_)
            .join_span(&self.endif_span)
    }
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
    Block(Vec<Statement<'a>>), //TODO we should include begin and end
    If(If<'a>),
    Invalid,
    Union(Union<'a>),
    Replace(Replace<'a>),
    Case(CaseStatement<'a>),
}

impl<'a> Default for Statement<'a> {
    fn default() -> Self {
        Self::Invalid
    }
}

impl<'a> Spanned for Statement<'a> {
    fn span(&self) -> Span {
        match &self {
            Statement::CreateTable(v) => v.span(),
            Statement::CreateView(v) => v.span(),
            Statement::CreateTrigger(v) => v.span(),
            Statement::CreateFunction(v) => v.span(),
            Statement::Select(v) => v.span(),
            Statement::Delete(v) => v.span(),
            Statement::Insert(v) => v.span(),
            Statement::Update(v) => v.span(),
            Statement::DropTable(v) => v.span(),
            Statement::DropFunction(v) => v.span(),
            Statement::DropProcedure(v) => v.span(),
            Statement::DropEvent(v) => v.span(),
            Statement::DropDatabase(v) => v.span(),
            Statement::DropServer(v) => v.span(),
            Statement::DropTrigger(v) => v.span(),
            Statement::DropView(v) => v.span(),
            Statement::Set(v) => v.span(),
            Statement::AlterTable(v) => v.span(),
            Statement::Block(v) => v.opt_span().unwrap(),
            Statement::If(v) => v.span(),
            Statement::Invalid => todo!(),
            Statement::Union(v) => v.span(),
            Statement::Replace(v) => v.span(),
            Statement::Case(v) => v.span(),
        }
    }
}

pub(crate) fn parse_statement<'a>(
    parser: &mut Parser<'a>,
) -> Result<Option<Statement<'a>>, ParseError> {
    Ok(match &parser.token {
        Token::Ident(_, Keyword::CREATE) => Some(parse_create(parser)?),
        Token::Ident(_, Keyword::DROP) => Some(parse_drop(parser)?),
        Token::Ident(_, Keyword::SELECT) | Token::LParen => Some(parse_compound_query(parser)?),
        Token::Ident(_, Keyword::DELETE) => Some(Statement::Delete(parse_delete(parser)?)),
        Token::Ident(_, Keyword::INSERT) => Some(Statement::Insert(parse_insert(parser)?)),
        Token::Ident(_, Keyword::UPDATE) => Some(Statement::Update(parse_update(parser)?)),
        Token::Ident(_, Keyword::SET) => Some(Statement::Set(parse_set(parser)?)),
        Token::Ident(_, Keyword::BEGIN) => Some(Statement::Block(parse_block(parser)?)),
        Token::Ident(_, Keyword::IF) => Some(Statement::If(parse_if(parser)?)),
        Token::Ident(_, Keyword::ALTER) => Some(parse_alter(parser)?),
        Token::Ident(_, Keyword::REPLACE) => Some(Statement::Replace(parse_replace(parser)?)),
        Token::Ident(_, Keyword::CASE) => Some(Statement::Case(parse_case_statement(parser)?)),
        _ => None,
    })
}

#[derive(Clone, Debug)]
pub struct WhenStatement<'a> {
    pub when_span: Span,
    pub when: Expression<'a>,
    pub then_span: Span,
    pub then: Vec<Statement<'a>>,
}

impl<'a> Spanned for WhenStatement<'a> {
    fn span(&self) -> Span {
        self.when_span
            .join_span(&self.when)
            .join_span(&self.then_span)
            .join_span(&self.then)
    }
}

#[derive(Clone, Debug)]
pub struct CaseStatement<'a> {
    pub case_span: Span,
    pub value: Box<Expression<'a>>,
    pub whens: Vec<WhenStatement<'a>>,
    pub else_: Option<(Span, Vec<Statement<'a>>)>,
    pub end_span: Span,
}

impl<'a> Spanned for CaseStatement<'a> {
    fn span(&self) -> Span {
        self.case_span
            .join_span(&self.value)
            .join_span(&self.whens)
            .join_span(&self.else_)
            .join_span(&self.end_span)
    }
}

pub(crate) fn parse_case_statement<'a>(
    parser: &mut Parser<'a>,
) -> Result<CaseStatement<'a>, ParseError> {
    let case_span = parser.consume_keyword(Keyword::CASE)?;
    let value = Box::new(parse_expression(parser, false)?);
    let mut whens = Vec::new();
    let mut else_ = None;
    parser.recovered(
        "'END'",
        &|t| matches!(t, Token::Ident(_, Keyword::END)),
        |parser| {
            loop {
                let when_span = parser.consume_keyword(Keyword::WHEN)?;
                let when = parse_expression(parser, false)?;
                let then_span = parser.consume_keyword(Keyword::THEN)?;
                let mut then = Vec::new();
                parse_statement_list(parser, &mut then)?;
                whens.push(WhenStatement {
                    when_span,
                    when,
                    then_span,
                    then,
                });
                if !matches!(parser.token, Token::Ident(_, Keyword::WHEN)) {
                    break;
                }
            }
            if let Some(span) = parser.skip_keyword(Keyword::ELSE) {
                let mut e = Vec::new();
                parse_statement_list(parser, &mut e)?;
                else_ = Some((span, e))
            };
            Ok(())
        },
    )?;
    let end_span = parser.consume_keyword(Keyword::END)?;
    Ok(CaseStatement {
        case_span,
        value,
        whens,
        else_,
        end_span,
    })
}

pub(crate) fn parse_compound_query_bottom<'a>(
    parser: &mut Parser<'a>,
) -> Result<Statement<'a>, ParseError> {
    match &parser.token {
        Token::LParen => {
            parser.consume_token(Token::LParen)?;
            let s = parser.recovered("')'", &|t| t == &Token::RParen, |parser| {
                parse_compound_query(parser)
            })?;
            parser.consume_token(Token::RParen)?;
            Ok(s)
        }
        Token::Ident(_, Keyword::SELECT) => Ok(Statement::Select(parse_select(parser)?)),
        _ => parser.expected_failure("'SELECET' or '('")?,
    }
}

#[derive(Clone, Debug)]
pub enum UnionType {
    All(Span),
    Distinct(Span),
    Default,
}

impl OptSpanned for UnionType {
    fn opt_span(&self) -> Option<Span> {
        match &self {
            UnionType::All(v) => v.opt_span(),
            UnionType::Distinct(v) => v.opt_span(),
            UnionType::Default => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnionWith<'a> {
    pub union_span: Span,
    pub union_type: UnionType,
    pub union_statement: Box<Statement<'a>>,
}

impl<'a> Spanned for UnionWith<'a> {
    fn span(&self) -> Span {
        self.union_span
            .join_span(&self.union_type)
            .join_span(&self.union_statement)
    }
}

#[derive(Clone, Debug)]
pub struct Union<'a> {
    pub left: Box<Statement<'a>>,
    pub with: Vec<UnionWith<'a>>,
    pub order_by: Option<(Span, Vec<(Expression<'a>, OrderFlag)>)>,
    pub limit: Option<(Span, Option<Expression<'a>>, Expression<'a>)>,
}

impl<'a> Spanned for Union<'a> {
    fn span(&self) -> Span {
        self.left
            .join_span(&self.with)
            .join_span(&self.order_by)
            .join_span(&self.limit)
    }
}

pub(crate) fn parse_compound_query<'a>(
    parser: &mut Parser<'a>,
) -> Result<Statement<'a>, ParseError> {
    let q = parse_compound_query_bottom(parser)?;
    if !matches!(parser.token, Token::Ident(_, Keyword::UNION)) {
        return Ok(q);
    };
    let mut with = Vec::new();
    loop {
        let union_span = parser.consume_keyword(Keyword::UNION)?;
        let union_type = match &parser.token {
            Token::Ident(_, Keyword::ALL) => UnionType::All(parser.consume_keyword(Keyword::ALL)?),
            Token::Ident(_, Keyword::DISTINCT) => {
                UnionType::Distinct(parser.consume_keyword(Keyword::DISTINCT)?)
            }
            _ => UnionType::Default,
        };
        let union_statement = Box::new(parse_compound_query_bottom(parser)?);
        with.push(UnionWith {
            union_span,
            union_type,
            union_statement,
        });
        if !matches!(parser.token, Token::Ident(_, Keyword::UNION)) {
            break;
        }
    }

    let order_by = if let Some(span) = parser.skip_keyword(Keyword::ORDER) {
        let span = parser.consume_keyword(Keyword::BY)?.join_span(&span);
        let mut order = Vec::new();
        loop {
            let e = parse_expression(parser, false)?;
            let f = match &parser.token {
                Token::Ident(_, Keyword::ASC) => OrderFlag::Asc(parser.consume()),
                Token::Ident(_, Keyword::DESC) => OrderFlag::Desc(parser.consume()),
                _ => OrderFlag::None,
            };
            order.push((e, f));
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Some((span, order))
    } else {
        None
    };

    let limit = if let Some(span) = parser.skip_keyword(Keyword::LIMIT) {
        let n = parse_expression(parser, true)?;
        match parser.token {
            Token::Comma => {
                parser.consume();
                Some((span, Some(n), parse_expression(parser, true)?))
            }
            Token::Ident(_, Keyword::OFFSET) => {
                parser.consume();
                Some((span, Some(parse_expression(parser, true)?), n))
            }
            _ => Some((span, None, n)),
        }
    } else {
        None
    };

    Ok(Statement::Union(Union {
        left: Box::new(q),
        with,
        order_by,
        limit,
    }))
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
