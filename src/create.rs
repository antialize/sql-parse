use std::borrow::Cow;

use crate::{
    data_type::parse_data_type,
    expression::parse_expression,
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    select::{parse_select, Select},
    statement::parse_statement,
    DataType, Span, Spanned, Statement,
};

#[derive(Clone, Debug)]
pub enum TableOption<'a> {
    AutoExtendSize {
        identifier: Span,
        value: (&'a str, Span),
    },
    AutoIncrement {
        identifier: Span,
        value: (&'a str, Span),
    },
    AvgRowLength {
        identifier: Span,
        value: (&'a str, Span),
    },
    CharSet {
        identifier: Span,
        value: (&'a str, Span),
    },
    DefaultCharSet {
        identifier: Span,
        value: (&'a str, Span),
    },
    Checksum {
        identifier: Span,
        value: (bool, Span),
    },
    Collate {
        identifier: Span,
        value: (&'a str, Span),
    },
    DefaultCollate {
        identifier: Span,
        value: (&'a str, Span),
    },
    Comment {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    Compression {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    Connection {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    DataDirectory {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    IndexDirectory {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    DelayKeyWrite {
        identifier: Span,
        value: (bool, Span),
    },
    Encryption {
        identifier: Span,
        value: (bool, Span),
    },
    Engine {
        identifier: Span,
        value: (&'a str, Span),
    },
    EngineAttribute {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    InsertMethod {
        identifier: Span,
        value: (&'a str, Span),
    },
    KeyBlockSize {
        identifier: Span,
        value: (usize, Span),
    },
    MaxRows {
        identifier: Span,
        value: (usize, Span),
    },
    MinRows {
        identifier: Span,
        value: (usize, Span),
    },
    // PACK_KEYS
    Password {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    RowFormat {
        identifier: Span,
        value: (&'a str, Span),
    },
    SecondaryEngineAttribute {
        identifier: Span,
        value: (Cow<'a, str>, Span),
    },
    //StatsAutoRecalc
    //StatsPersistance
    //StatsSamplePages
    //TABLESPACE
    //UNION
}

#[derive(Clone, Debug)]
pub enum CreateDefinition<'a> {
    ColumnDefinition {
        identifier: (&'a str, Span),
        data_type: DataType<'a>,
    },
}

#[derive(Clone, Debug)]
pub enum CreateAlgorithm {
    Undefined(Span),
    Merge(Span),
    TempTable(Span),
}

#[derive(Clone, Debug)]
pub enum CreateOption<'a> {
    OrReplace(Span),
    Temporary(Span),
    Algorithm(Span, CreateAlgorithm),
    Definer {
        definer_span: Span,
        user: (&'a str, Span),
        host: (&'a str, Span),
    },
    SqlSecurityDefiner(Span, Span),
    SqlSecurityUser(Span, Span),
}
#[derive(Clone, Debug)]
pub struct CreateTable<'a> {
    pub create_span: Span,
    pub create_options: Vec<CreateOption<'a>>,
    pub table_span: Span,
    pub identifier: (&'a str, Span),
    pub if_not_exists: Option<Span>,
    pub create_definitions: Vec<CreateDefinition<'a>>,
    pub options: Vec<TableOption<'a>>,
}

#[derive(Clone, Debug)]
pub struct CreateView<'a> {
    pub create_span: Span,
    pub create_options: Vec<CreateOption<'a>>,
    pub view_span: Span,
    pub if_not_exists: Option<Span>,
    pub name: (&'a str, Span),
    pub as_span: Span,
    pub select: Select<'a>,
}

pub(crate) fn parse_create_definition<'a>(
    parser: &mut Parser<'a>,
) -> Result<CreateDefinition<'a>, ParseError> {
    match &parser.token {
        Token::Ident(_, _) => Ok(CreateDefinition::ColumnDefinition {
            identifier: parser.consume_plain_identifier()?,
            data_type: parse_data_type(parser)?,
        }),
        _ => parser.expected_failure("identifier"),
    }
}

fn parse_create_view<'a>(
    parser: &mut Parser<'a>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let view_span = parser.consume_keyword(Keyword::VIEW)?;

    let if_not_exists = if let Some(if_) = parser.skip_keyword(Keyword::IF) {
        parser.consume_keyword(Keyword::NOT)?;
        Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&if_))
    } else {
        None
    };

    let name = parser.consume_plain_identifier()?;
    // TODO (column_list)

    let as_span = parser.consume_keyword(Keyword::AS)?;

    let select = parse_select(parser)?;

    // TODO [WITH [CASCADED | LOCAL] CHECK OPTION]

    Ok(Statement::CreateView(CreateView {
        create_span,
        create_options,
        view_span,
        if_not_exists,
        name,
        as_span,
        select,
    }))
}

#[derive(Clone, Debug)]
pub enum FunctionCharacteristic<'a> {
    LanguageSql(Span),
    NotDeterministic(Span),
    Deterministic(Span),
    ContainsSql(Span),
    NoSql(Span),
    ReadsSqlData(Span),
    ModifiesSqlData(Span),
    SqlSecurityDefiner(Span),
    SqlSecurityUser(Span),
    Comment((Cow<'a, str>, Span)),
}

#[derive(Clone, Debug)]
pub struct CreateFunction<'a> {
    pub create_span: Span,
    pub create_options: Vec<CreateOption<'a>>,
    pub function_span: Span,
    pub if_not_exists: Option<Span>,
    pub name: (&'a str, Span),
    pub params: Vec<((&'a str, Span), DataType<'a>)>,
    pub returns_span: Span,
    pub return_type: DataType<'a>,
    pub characteristics: Vec<FunctionCharacteristic<'a>>,
    pub return_span: Span,
    pub return_: Box<Statement<'a>>,
}

fn parse_create_function<'a>(
    parser: &mut Parser<'a>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let function_span = parser.consume_keyword(Keyword::FUNCTION)?;

    let if_not_exists = if let Some(if_) = parser.skip_keyword(Keyword::IF) {
        Some(
            parser
                .consume_keywords(&[Keyword::NOT, Keyword::EXISTS])?
                .join_span(&if_),
        )
    } else {
        None
    };

    let name = parser.consume_plain_identifier()?;
    let mut params = Vec::new();
    parser.consume_token(Token::LParen)?;
    parser.recovered("')'", &|t| t == &Token::RParen, |parser| {
        loop {
            let name = parser.consume_plain_identifier()?;
            let type_ = parse_data_type(parser)?;
            params.push((name, type_));
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Ok(())
    })?;
    parser.consume_token(Token::RParen)?;
    let returns_span = parser.consume_keyword(Keyword::RETURNS)?;
    let return_type = parse_data_type(parser)?;
    let mut characteristics = Vec::new();
    loop {
        let f = match &parser.token {
            Token::Ident(_, Keyword::LANGUAGE) => FunctionCharacteristic::LanguageSql(
                parser.consume_keywords(&[Keyword::LANGUAGE, Keyword::SQL])?,
            ),
            Token::Ident(_, Keyword::NOT) => FunctionCharacteristic::NotDeterministic(
                parser.consume_keywords(&[Keyword::NOT, Keyword::DETERMINISTIC])?,
            ),
            Token::Ident(_, Keyword::DETERMINISTIC) => FunctionCharacteristic::Deterministic(
                parser.consume_keyword(Keyword::DETERMINISTIC)?,
            ),
            Token::Ident(_, Keyword::CONTAINS) => FunctionCharacteristic::ContainsSql(
                parser.consume_keywords(&[Keyword::CONTAINS, Keyword::SQL])?,
            ),
            Token::Ident(_, Keyword::NO) => FunctionCharacteristic::NoSql(
                parser.consume_keywords(&[Keyword::NO, Keyword::SQL])?,
            ),
            Token::Ident(_, Keyword::READS) => {
                FunctionCharacteristic::ReadsSqlData(parser.consume_keywords(&[
                    Keyword::READS,
                    Keyword::SQL,
                    Keyword::DATA,
                ])?)
            }
            Token::Ident(_, Keyword::MODIFIES) => {
                FunctionCharacteristic::ModifiesSqlData(parser.consume_keywords(&[
                    Keyword::MODIFIES,
                    Keyword::SQL,
                    Keyword::DATA,
                ])?)
            }
            Token::Ident(_, Keyword::COMMENT) => {
                parser.consume_keyword(Keyword::COMMENT)?;
                FunctionCharacteristic::Comment(parser.consume_string()?)
            }
            Token::Ident(_, Keyword::SQL) => {
                let span = parser.consume_keywords(&[Keyword::SQL, Keyword::SECURITY])?;
                match &parser.token {
                    Token::Ident(_, Keyword::DEFINER) => {
                        FunctionCharacteristic::SqlSecurityDefiner(
                            parser.consume_keyword(Keyword::DEFINER)?.join_span(&span),
                        )
                    }
                    Token::Ident(_, Keyword::USER) => FunctionCharacteristic::SqlSecurityUser(
                        parser.consume_keyword(Keyword::USER)?.join_span(&span),
                    ),
                    _ => parser.expected_failure("'DEFINER' or 'USER'")?,
                }
            }
            _ => break,
        };
        characteristics.push(f);
    }

    let return_span = parser.consume_keyword(Keyword::RETURN)?;
    let return_ = match parse_statement(parser)? {
        Some(v) => Box::new(v),
        None => parser.expected_failure("statement")?,
    };

    Ok(Statement::CreateFunction(CreateFunction {
        create_span,
        create_options,
        function_span,
        if_not_exists,
        name,
        params,
        returns_span,
        return_type,
        characteristics,
        return_span,
        return_,
    }))
}

#[derive(Clone, Debug)]

pub enum TriggerTime {
    Before(Span),
    After(Span),
}

#[derive(Clone, Debug)]
pub enum TriggerEvent {
    Update(Span),
    Insert(Span),
    Delete(Span),
}

#[derive(Clone, Debug)]
pub struct CreateTrigger<'a> {
    pub create_span: Span,
    pub create_options: Vec<CreateOption<'a>>,
    pub trigger_span: Span,
    pub name: (&'a str, Span),
    pub trigger_time: TriggerTime,
    pub trigger_event: TriggerEvent,
    pub on_span: Span,
    pub table: (&'a str, Span),
    pub for_each_row_span: Span,
    pub statement: Box<Statement<'a>>,
}

fn parse_create_trigger<'a>(
    parser: &mut Parser<'a>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let trigger_span = parser.consume_keyword(Keyword::TRIGGER)?;

    let if_not_exists = if let Some(if_) = parser.skip_keyword(Keyword::IF) {
        parser.consume_keyword(Keyword::NOT)?;
        Some(parser.consume_keyword(Keyword::EXISTS)?.join_span(&if_))
    } else {
        None
    };

    let name = parser.consume_plain_identifier()?;

    let trigger_time = match &parser.token {
        Token::Ident(_, Keyword::AFTER) => {
            TriggerTime::After(parser.consume_keyword(Keyword::AFTER)?)
        }
        Token::Ident(_, Keyword::BEFORE) => {
            TriggerTime::Before(parser.consume_keyword(Keyword::BEFORE)?)
        }
        _ => parser.expected_failure("'BEFORE' or 'AFTER'")?,
    };

    let trigger_event = match &parser.token {
        Token::Ident(_, Keyword::UPDATE) => {
            TriggerEvent::Update(parser.consume_keyword(Keyword::UPDATE)?)
        }
        Token::Ident(_, Keyword::INSERT) => {
            TriggerEvent::Insert(parser.consume_keyword(Keyword::INSERT)?)
        }
        Token::Ident(_, Keyword::DELETE) => {
            TriggerEvent::Delete(parser.consume_keyword(Keyword::DELETE)?)
        }
        _ => parser.expected_failure("'UPDATE' or 'INSERT' or 'DELETE'")?,
    };

    let on_span = parser.consume_keyword(Keyword::ON)?;

    let table = parser.consume_plain_identifier()?;

    let for_each_row_span = parser
        .consume_keyword(Keyword::FOR)?
        .join_span(&parser.consume_keyword(Keyword::EACH)?)
        .join_span(&parser.consume_keyword(Keyword::ROW)?);

    // TODO [{ FOLLOWS | PRECEDES } other_trigger_name ]

    let statement = match parse_statement(parser)? {
        Some(v) => v,
        None => parser.expected_failure("statement")?,
    };

    Ok(Statement::CreateTrigger(CreateTrigger {
        create_span,
        create_options,
        trigger_span,
        name,
        trigger_time,
        trigger_event,
        on_span,
        table,
        for_each_row_span,
        statement: Box::new(statement),
    }))
}

fn parse_create_table<'a>(
    parser: &mut Parser<'a>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let table_span = parser.consume_keyword(Keyword::TABLE)?;

    let mut identifier = ("", 0..0);
    let mut if_not_exists = None;

    parser.recovered("'('", &|t| t == &Token::LParen, |parser| {
        if let Some(if_) = parser.skip_keyword(Keyword::IF) {
            parser.consume_keyword(Keyword::NOT)?;
            if_not_exists = Some(if_.start..parser.consume_keyword(Keyword::EXISTS)?.end);
        }
        identifier = parser.consume_plain_identifier()?;
        Ok(())
    })?;

    parser.consume_token(Token::LParen)?;

    let mut create_definitions = Vec::new();
    loop {
        parser.recovered(
            "')' or ','",
            &|t| matches!(t, Token::RParen | Token::Comma),
            |parser| {
                create_definitions.push(parse_create_definition(parser)?);
                Ok(())
            },
        )?;
        if matches!(parser.token, Token::RParen) {
            break;
        }
        parser.consume_token(Token::Comma)?;
    }
    parser.consume_token(Token::RParen)?;

    let mut options = Vec::new();
    let delimiter = parser.delimiter.clone();
    parser.recovered(
        delimiter.name(),
        &|t| t == &Token::Eof || t == &delimiter,
        |parser| {
            loop {
                let identifier = parser.span.clone();
                match &parser.token {
                    Token::Ident(_, Keyword::ENGINE) => {
                        parser.consume_keyword(Keyword::ENGINE)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::Engine {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                    }
                    Token::Ident(_, Keyword::DEFAULT) => {
                        parser.consume_keyword(Keyword::DEFAULT)?;
                        match &parser.token {
                            Token::Ident(_, Keyword::CHARSET) => {
                                parser.consume_keyword(Keyword::CHARSET)?;
                                parser.skip_token(Token::Eq);
                                options.push(TableOption::DefaultCharSet {
                                    identifier,
                                    value: parser.consume_plain_identifier()?,
                                });
                            }
                            Token::Ident(_, Keyword::COLLATE) => {
                                parser.consume_keyword(Keyword::COLLATE)?;
                                parser.skip_token(Token::Eq);
                                options.push(TableOption::DefaultCollate {
                                    identifier,
                                    value: parser.consume_plain_identifier()?,
                                });
                            }
                            _ => parser.expected_failure("'CHARSET' or 'COLLATE'")?,
                        }
                    }
                    Token::Ident(_, Keyword::CHARSET) => {
                        parser.consume_keyword(Keyword::CHARSET)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::CharSet {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                    }
                    Token::Ident(_, Keyword::COLLATE) => {
                        parser.consume_keyword(Keyword::COLLATE)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::Collate {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                    }
                    Token::Ident(_, Keyword::ROW_FORMAT) => {
                        parser.consume_keyword(Keyword::ROW_FORMAT)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::RowFormat {
                            identifier,
                            value: parser.consume_plain_identifier()?,
                        });
                        //TODO validate raw format is in the keyword set
                    }
                    Token::Ident(_, Keyword::COMMENT) => {
                        parser.consume_keyword(Keyword::COMMENT)?;
                        parser.skip_token(Token::Eq);
                        options.push(TableOption::Comment {
                            identifier,
                            value: parser.consume_string()?,
                        });
                    }
                    t if t == &parser.delimiter => break,
                    Token::Eof => break,
                    _ => {
                        parser.expected_failure("table option or delimiter")?;
                    }
                }
            }
            Ok(())
        },
    )?;

    Ok(Statement::CreateTable(CreateTable {
        create_span,
        create_options,
        table_span,
        identifier,
        if_not_exists,
        options,
        create_definitions,
    }))
}

pub(crate) fn parse_create<'a>(parser: &mut Parser<'a>) -> Result<Statement<'a>, ParseError> {
    let create_span = parser.span.clone();
    parser.consume_keyword(Keyword::CREATE)?;

    let mut create_options = Vec::new();
    const CREATABLE: &str = "'TABLE' | 'VIEW' | 'TRIGGER' | 'FUNCTION'";

    parser.recovered(
        CREATABLE,
        &|t| {
            matches!(
                t,
                Token::Ident(
                    _,
                    Keyword::TABLE | Keyword::VIEW | Keyword::TRIGGER | Keyword::FUNCTION
                )
            )
        },
        |parser| {
            loop {
                let v = match &parser.token {
                    Token::Ident(_, Keyword::OR) => CreateOption::OrReplace(
                        parser
                            .consume_keyword(Keyword::OR)?
                            .join_span(&parser.consume_keyword(Keyword::REPLACE)?),
                    ),
                    Token::Ident(_, Keyword::TEMPORARY) => {
                        CreateOption::Temporary(parser.consume_keyword(Keyword::TEMPORARY)?)
                    }
                    Token::Ident(_, Keyword::ALGORITHM) => {
                        let algorithm_span = parser.consume_keyword(Keyword::ALGORITHM)?;
                        parser.consume_token(Token::Eq)?;
                        let algorithm = match &parser.token {
                            Token::Ident(_, Keyword::UNDEFINED) => CreateAlgorithm::Undefined(
                                parser.consume_keyword(Keyword::UNDEFINED)?,
                            ),
                            Token::Ident(_, Keyword::MERGE) => {
                                CreateAlgorithm::Merge(parser.consume_keyword(Keyword::MERGE)?)
                            }
                            Token::Ident(_, Keyword::TEMPTABLE) => CreateAlgorithm::TempTable(
                                parser.consume_keyword(Keyword::TEMPTABLE)?,
                            ),
                            _ => parser.expected_failure("'UNDEFINED', 'MERGE' or 'TEMPTABLE'")?,
                        };
                        CreateOption::Algorithm(algorithm_span, algorithm)
                    }
                    Token::Ident(_, Keyword::DEFINER) => {
                        let definer_span = parser.consume_keyword(Keyword::DEFINER)?;
                        parser.consume_token(Token::Eq)?;
                        // TODO user | CURRENT_USER | role | CURRENT_ROLE
                        let user = parser.consume_plain_identifier()?;
                        parser.consume_token(Token::At)?;
                        let host = parser.consume_plain_identifier()?;
                        CreateOption::Definer {
                            definer_span,
                            user,
                            host,
                        }
                    }
                    Token::Ident(_, Keyword::SQL) => {
                        let sql_security = parser
                            .consume_keyword(Keyword::SQL)?
                            .join_span(&parser.consume_keyword(Keyword::SECURITY)?);
                        match &parser.token {
                            Token::Ident(_, Keyword::DEFINER) => CreateOption::SqlSecurityDefiner(
                                sql_security,
                                parser.consume_keyword(Keyword::DEFINER)?,
                            ),
                            Token::Ident(_, Keyword::USER) => CreateOption::SqlSecurityUser(
                                sql_security,
                                parser.consume_keyword(Keyword::USER)?,
                            ),
                            _ => parser.expected_failure("'DEFINER', 'USER'")?,
                        }
                    }
                    _ => break,
                };
                create_options.push(v);
            }
            Ok(())
        },
    )?;

    match &parser.token {
        Token::Ident(_, Keyword::TABLE) => parse_create_table(parser, create_span, create_options),
        Token::Ident(_, Keyword::VIEW) => parse_create_view(parser, create_span, create_options),
        Token::Ident(_, Keyword::FUNCTION) => {
            parse_create_function(parser, create_span, create_options)
        }
        Token::Ident(_, Keyword::TRIGGER) => {
            parse_create_trigger(parser, create_span, create_options)
        }
        _ => parser.expected_failure(CREATABLE),
    }
}
