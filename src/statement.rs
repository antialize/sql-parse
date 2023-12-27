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

use alloc::{
    boxed::Box,
    format,
    string::ToString,
    vec::{self, Vec},
};

use crate::{
    alter::{parse_alter, AlterTable},
    create::{
        parse_create, CreateFunction, CreateIndex, CreateTable, CreateTrigger, CreateTypeEnum,
        CreateView,
    },
    delete::{parse_delete, Delete},
    drop::{
        parse_drop, DropDatabase, DropEvent, DropFunction, DropIndex, DropProcedure, DropServer,
        DropTable, DropTrigger, DropView,
    },
    expression::{parse_expression, Expression},
    insert_replace::{parse_insert_replace, InsertReplace},
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    qualified_name::parse_qualified_name,
    select::{parse_select, OrderFlag, Select},
    span::OptSpanned,
    truncate::TruncateTable,
    update::{parse_update, Update},
    Identifier, Span, Spanned,
};

#[derive(Clone, Debug)]
pub struct Set<'a> {
    pub set_span: Span,
    pub values: Vec<(Identifier<'a>, Expression<'a>)>,
}

impl<'a> Spanned for Set<'a> {
    fn span(&self) -> Span {
        self.set_span.join_span(&self.values)
    }
}

fn parse_set<'a>(parser: &mut Parser<'a, '_>) -> Result<Set<'a>, ParseError> {
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
    parser: &mut Parser<'a, '_>,
    out: &mut Vec<Statement<'a>>,
) -> Result<(), ParseError> {
    loop {
        while parser.skip_token(Token::SemiColon).is_some() {}
        let stdin = match parse_statement(parser)? {
            Some(v) => {
                let stdin = v.reads_from_stdin();
                out.push(v);
                stdin
            }
            None => break,
        };
        if !matches!(parser.token, Token::SemiColon) {
            break;
        }
        if stdin {
            let (s, span) = parser.read_from_stdin_and_next();
            out.push(Statement::Stdin(s, span));
        } else {
            parser.consume_token(Token::SemiColon)?;
        }
    }
    Ok(())
}

fn parse_statement_list<'a>(
    parser: &mut Parser<'a, '_>,
    out: &mut Vec<Statement<'a>>,
) -> Result<(), ParseError> {
    let old_delimiter = core::mem::replace(&mut parser.delimiter, Token::SemiColon);
    let r = parse_statement_list_inner(parser, out);
    parser.delimiter = old_delimiter;
    r
}

fn parse_begin(parser: &mut Parser<'_, '_>) -> Result<Span, ParseError> {
    parser.consume_keyword(Keyword::BEGIN)
}

fn parse_end(parser: &mut Parser<'_, '_>) -> Result<Span, ParseError> {
    parser.consume_keyword(Keyword::END)
}

fn parse_start<'a>(parser: &mut Parser<'a, '_>) -> Result<Statement<'a>, ParseError> {
    Ok(Statement::StartTransaction(parser.consume_keywords(&[
        Keyword::START,
        Keyword::TRANSACTION,
    ])?))
}

fn parse_commit(parser: &mut Parser<'_, '_>) -> Result<Span, ParseError> {
    parser.consume_keyword(Keyword::COMMIT)
}

fn parse_block<'a>(parser: &mut Parser<'a, '_>) -> Result<Vec<Statement<'a>>, ParseError> {
    parser.consume_keyword(Keyword::BEGIN)?;
    let mut ans = Vec::new();
    parser.recovered(
        "'END' | 'EXCEPTION'",
        &|e| {
            matches!(
                e,
                Token::Ident(_, Keyword::END) | Token::Ident(_, Keyword::EXCEPTION)
            )
        },
        |parser| parse_statement_list(parser, &mut ans),
    )?;
    if let Some(_exception_span) = parser.skip_keyword(Keyword::EXCEPTION) {
        while let Some(_when_span) = parser.skip_keyword(Keyword::WHEN) {
            parser.consume_plain_identifier()?;
            parser.consume_keyword(Keyword::THEN)?;
            parse_expression(parser, true)?;
            parser.consume_token(Token::SemiColon)?;
        }
    }
    parser.consume_keyword(Keyword::END)?;
    Ok(ans)
}

/// Condition in if statement
#[derive(Clone, Debug)]
pub struct IfCondition<'a> {
    /// Span of "ELSEIF" if specified
    pub elseif_span: Option<Span>,
    /// Expression that must be true for `then` to be executed
    pub search_condition: Expression<'a>,
    /// Span of "THEN"
    pub then_span: Span,
    /// List of statement to be executed if `search_condition` is true
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

/// If statement
#[derive(Clone, Debug)]
pub struct If<'a> {
    /// Span of "IF"
    pub if_span: Span,
    // List of if a then v parts
    pub conditions: Vec<IfCondition<'a>>,
    /// Span of "ELSE" and else Statement if specified
    pub else_: Option<(Span, Vec<Statement<'a>>)>,
    /// Span of "ENDIF"
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

fn parse_if<'a>(parser: &mut Parser<'a, '_>) -> Result<If<'a>, ParseError> {
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

/// SQL statement
#[derive(Clone, Debug)]
pub enum Statement<'a> {
    CreateIndex(CreateIndex<'a>),
    CreateTable(CreateTable<'a>),
    CreateView(CreateView<'a>),
    CreateTrigger(CreateTrigger<'a>),
    CreateFunction(CreateFunction<'a>),
    Select(Select<'a>),
    Delete(Delete<'a>),
    InsertReplace(InsertReplace<'a>),
    Update(Update<'a>),
    DropIndex(DropIndex<'a>),
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
    Begin(Span),
    End(Span),
    Commit(Span),
    StartTransaction(Span),
    If(If<'a>),
    /// Invalid statement produced after recovering from parse error
    Invalid(Span),
    Union(Union<'a>),
    Case(CaseStatement<'a>),
    Copy(Copy<'a>),
    Stdin(&'a str, Span),
    CreateTypeEnum(CreateTypeEnum<'a>),
    Do(Vec<Statement<'a>>),
    TruncateTable(TruncateTable<'a>),
}

impl<'a> Spanned for Statement<'a> {
    fn span(&self) -> Span {
        match &self {
            Statement::CreateIndex(v) => v.span(),
            Statement::CreateTable(v) => v.span(),
            Statement::CreateView(v) => v.span(),
            Statement::CreateTrigger(v) => v.span(),
            Statement::CreateFunction(v) => v.span(),
            Statement::Select(v) => v.span(),
            Statement::Delete(v) => v.span(),
            Statement::InsertReplace(v) => v.span(),
            Statement::Update(v) => v.span(),
            Statement::DropIndex(v) => v.span(),
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
            Statement::Block(v) => v.opt_span().expect("Span of block"),
            Statement::If(v) => v.span(),
            Statement::Invalid(v) => v.span(),
            Statement::Union(v) => v.span(),
            Statement::Case(v) => v.span(),
            Statement::Copy(v) => v.span(),
            Statement::Stdin(_, s) => s.clone(),
            Statement::Begin(s) => s.clone(),
            Statement::End(s) => s.clone(),
            Statement::Commit(s) => s.clone(),
            Statement::StartTransaction(s) => s.clone(),
            Statement::CreateTypeEnum(v) => v.span(),
            Statement::Do(v) => v.opt_span().expect("Span of block"),
            Statement::TruncateTable(v) => v.span(),
        }
    }
}

impl Statement<'_> {
    fn reads_from_stdin(&self) -> bool {
        match self {
            Statement::Copy(v) => v.reads_from_stdin(),
            _ => false,
        }
    }
}

pub(crate) fn parse_statement<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<Option<Statement<'a>>, ParseError> {
    Ok(match &parser.token {
        Token::Ident(_, Keyword::CREATE) => Some(parse_create(parser)?),
        Token::Ident(_, Keyword::DROP) => Some(parse_drop(parser)?),
        Token::Ident(_, Keyword::SELECT) | Token::LParen => Some(parse_compound_query(parser)?),
        Token::Ident(_, Keyword::DELETE) => Some(Statement::Delete(parse_delete(parser)?)),
        Token::Ident(_, Keyword::INSERT | Keyword::REPLACE) => {
            Some(Statement::InsertReplace(parse_insert_replace(parser)?))
        }
        Token::Ident(_, Keyword::UPDATE) => Some(Statement::Update(parse_update(parser)?)),
        Token::Ident(_, Keyword::SET) => Some(Statement::Set(parse_set(parser)?)),
        Token::Ident(_, Keyword::BEGIN) => Some(if parser.permit_compound_statements {
            Statement::Block(parse_block(parser)?)
        } else {
            Statement::Begin(parse_begin(parser)?)
        }),
        Token::Ident(_, Keyword::END) if !parser.permit_compound_statements => {
            Some(Statement::End(parse_end(parser)?))
        }
        Token::Ident(_, Keyword::START) => Some(parse_start(parser)?),
        Token::Ident(_, Keyword::COMMIT) => Some(Statement::Commit(parse_commit(parser)?)),
        Token::Ident(_, Keyword::IF) => Some(Statement::If(parse_if(parser)?)),
        Token::Ident(_, Keyword::ALTER) => Some(parse_alter(parser)?),
        Token::Ident(_, Keyword::CASE) => Some(Statement::Case(parse_case_statement(parser)?)),
        Token::Ident(_, Keyword::COPY) => Some(Statement::Copy(parse_copy_statement(parser)?)),
        Token::Ident(_, Keyword::DO) => Some(parse_do(parser)?),
        Token::Ident(_, Keyword::TRUNCATE) => Some(parse_truncate_table(parser)?),
        _ => None,
    })
}

pub(crate) fn parse_truncate_table<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<Statement<'a>, ParseError> {
    let truncate_span = parser.consume_keyword(Keyword::TRUNCATE)?;
    let mut table_span = None;
    let table_name = loop {
        match parser.token {
            Token::Ident(_, k) => {
                if k == Keyword::TABLE {
                    if None == table_span {
                        table_span = Some(parser.span.clone());
                    } else {
                        parser.expected_failure(parser.token.name())?;
                    };
                } else if k == Keyword::QUOTED_IDENTIFIER || k == Keyword::NOT_A_KEYWORD {
                    break parse_qualified_name(parser)?;
                } else {
                    parser.expected_failure(parser.token.name())?;
                }
            }
            _ => parser.expected_failure(parser.token.name())?,
        };
        parser.next();
    };
    Ok(Statement::TruncateTable(TruncateTable {
        truncate_span,
        table_span,
        table_name,
    }))
}

pub(crate) fn parse_do<'a>(parser: &mut Parser<'a, '_>) -> Result<Statement<'a>, ParseError> {
    parser.consume_keyword(Keyword::DO)?;
    parser.consume_token(Token::DoubleDollar)?;
    let block = parse_block(parser)?;
    parser.consume_token(Token::DoubleDollar)?;
    Ok(Statement::Do(block))
}

/// When part of case statement
#[derive(Clone, Debug)]
pub struct WhenStatement<'a> {
    /// Span of "WHEN"
    pub when_span: Span,
    /// Expression who's match yields execution `then`
    pub when: Expression<'a>,
    /// Span of "THEN"
    pub then_span: Span,
    /// Statements to execute if `when` matches
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

/// Case statement
#[derive(Clone, Debug)]
pub struct CaseStatement<'a> {
    /// Span of "CASE"
    pub case_span: Span,
    /// Value to match against
    pub value: Box<Expression<'a>>,
    /// List of whens
    pub whens: Vec<WhenStatement<'a>>,
    /// Span of "ELSE" and statement to execute if specified
    pub else_: Option<(Span, Vec<Statement<'a>>)>,
    /// Span of "END"
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
    parser: &mut Parser<'a, '_>,
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

pub(crate) fn parse_copy_statement<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<Copy<'a>, ParseError> {
    let copy_span = parser.consume_keyword(Keyword::COPY)?;
    let table = parser.consume_plain_identifier()?;
    parser.consume_token(Token::LParen)?;
    let mut columns = Vec::new();
    if !matches!(parser.token, Token::RParen) {
        loop {
            parser.recovered(
                "')' or ','",
                &|t| matches!(t, Token::RParen | Token::Comma),
                |parser| {
                    columns.push(parser.consume_plain_identifier()?);
                    Ok(())
                },
            )?;
            if matches!(parser.token, Token::RParen) {
                break;
            }
            parser.consume_token(Token::Comma)?;
        }
    }
    parser.consume_token(Token::RParen)?;
    let from_span = parser.consume_keyword(Keyword::FROM)?;
    let stdin_span = parser.consume_keyword(Keyword::STDIN)?;

    Ok(Copy {
        copy_span,
        table,
        columns,
        from_span,
        stdin_span,
    })
}

pub(crate) fn parse_compound_query_bottom<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<Statement<'a>, ParseError> {
    match &parser.token {
        Token::LParen => {
            let lp = parser.consume_token(Token::LParen)?;
            let s = parser.recovered("')'", &|t| t == &Token::RParen, |parser| {
                Ok(Some(parse_compound_query(parser)?))
            })?;
            parser.consume_token(Token::RParen)?;
            Ok(s.unwrap_or(Statement::Invalid(lp)))
        }
        Token::Ident(_, Keyword::SELECT) => Ok(Statement::Select(parse_select(parser)?)),
        _ => parser.expected_failure("'SELECET' or '('")?,
    }
}

/// Type of union to perform
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

/// Right hand side of a union expression
#[derive(Clone, Debug)]
pub struct UnionWith<'a> {
    /// Span of "UNION"
    pub union_span: Span,
    /// Type of union to perform
    pub union_type: UnionType,
    /// Statement to union
    pub union_statement: Box<Statement<'a>>,
}

impl<'a> Spanned for UnionWith<'a> {
    fn span(&self) -> Span {
        self.union_span
            .join_span(&self.union_type)
            .join_span(&self.union_statement)
    }
}

/// Union statement
#[derive(Clone, Debug)]
pub struct Union<'a> {
    /// Left side of union
    pub left: Box<Statement<'a>>,
    /// List of things to union
    pub with: Vec<UnionWith<'a>>,
    /// Span of "ORDER BY", and list of ordering expressions and directions if specified
    pub order_by: Option<(Span, Vec<(Expression<'a>, OrderFlag)>)>,
    /// Span of "LIMIT", offset and count expressions if specified
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

#[derive(Clone, Debug)]
pub struct Copy<'a> {
    pub copy_span: Span,
    pub table: Identifier<'a>,
    pub columns: Vec<Identifier<'a>>,
    pub from_span: Span,
    pub stdin_span: Span,
}

impl<'a> Spanned for Copy<'a> {
    fn span(&self) -> Span {
        self.copy_span
            .join_span(&self.table)
            .join_span(&self.columns)
            .join_span(&self.from_span)
            .join_span(&self.stdin_span)
    }
}

impl<'a> Copy<'a> {
    fn reads_from_stdin(&self) -> bool {
        // There are COPY statements that don't read from STDIN,
        // but we don't support them in this parser - we only support FROM STDIN.
        true
    }
}

pub(crate) fn parse_compound_query<'a>(
    parser: &mut Parser<'a, '_>,
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

pub(crate) fn parse_statements<'a>(parser: &mut Parser<'a, '_>) -> Vec<Statement<'a>> {
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
                parser
                    .issues
                    .push(crate::Issue::warn("Unknown delimiter", &parser.span));
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
        let mut from_stdin = false;
        if let Ok(stmt) = stmt {
            from_stdin = stmt.reads_from_stdin();
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
        if from_stdin {
            let (s, span) = parser.read_from_stdin_and_next();
            ans.push(Statement::Stdin(s, span));
        } else {
            parser
                .consume_token(parser.delimiter.clone())
                .expect("Delimiter");
        }
    }
}
