use alloc::{boxed::Box, vec::Vec};

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
    expression::parse_expression,
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    select::{parse_select, Select},
    statement::parse_statement,
    DataType, Expression, Identifier, Issue, SString, Span, Spanned, Statement,
};

/// Options on created table
#[derive(Clone, Debug)]
pub enum TableOption<'a> {
    AutoExtendSize {
        identifier: Span,
        value: Identifier<'a>,
    },
    AutoIncrement {
        identifier: Span,
        value: Identifier<'a>,
    },
    AvgRowLength {
        identifier: Span,
        value: Identifier<'a>,
    },
    CharSet {
        identifier: Span,
        value: Identifier<'a>,
    },
    DefaultCharSet {
        identifier: Span,
        value: Identifier<'a>,
    },
    Checksum {
        identifier: Span,
        value: (bool, Span),
    },
    Collate {
        identifier: Span,
        value: Identifier<'a>,
    },
    DefaultCollate {
        identifier: Span,
        value: Identifier<'a>,
    },
    Comment {
        identifier: Span,
        value: SString<'a>,
    },
    Compression {
        identifier: Span,
        value: SString<'a>,
    },
    Connection {
        identifier: Span,
        value: SString<'a>,
    },
    DataDirectory {
        identifier: Span,
        value: SString<'a>,
    },
    IndexDirectory {
        identifier: Span,
        value: SString<'a>,
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
        value: Identifier<'a>,
    },
    EngineAttribute {
        identifier: Span,
        value: SString<'a>,
    },
    InsertMethod {
        identifier: Span,
        value: Identifier<'a>,
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
        value: SString<'a>,
    },
    RowFormat {
        identifier: Span,
        value: Identifier<'a>,
    },
    SecondaryEngineAttribute {
        identifier: Span,
        value: SString<'a>,
    },
    //StatsAutoRecalc
    //StatsPersistance
    //StatsSamplePages
    //TABLESPACE
    //UNION
}

impl<'a> Spanned for TableOption<'a> {
    fn span(&self) -> Span {
        match &self {
            TableOption::AutoExtendSize { identifier, value } => identifier.span().join_span(value),
            TableOption::AutoIncrement { identifier, value } => identifier.span().join_span(value),
            TableOption::AvgRowLength { identifier, value } => identifier.span().join_span(value),
            TableOption::CharSet { identifier, value } => identifier.span().join_span(value),
            TableOption::DefaultCharSet { identifier, value } => identifier.span().join_span(value),
            TableOption::Checksum { identifier, value } => identifier.span().join_span(value),
            TableOption::Collate { identifier, value } => identifier.span().join_span(value),
            TableOption::DefaultCollate { identifier, value } => identifier.span().join_span(value),
            TableOption::Comment { identifier, value } => identifier.span().join_span(value),
            TableOption::Compression { identifier, value } => identifier.span().join_span(value),
            TableOption::Connection { identifier, value } => identifier.span().join_span(value),
            TableOption::DataDirectory { identifier, value } => identifier.span().join_span(value),
            TableOption::IndexDirectory { identifier, value } => identifier.span().join_span(value),
            TableOption::DelayKeyWrite { identifier, value } => identifier.span().join_span(value),
            TableOption::Encryption { identifier, value } => identifier.span().join_span(value),
            TableOption::Engine { identifier, value } => identifier.span().join_span(value),
            TableOption::EngineAttribute { identifier, value } => {
                identifier.span().join_span(value)
            }
            TableOption::InsertMethod { identifier, value } => identifier.span().join_span(value),
            TableOption::KeyBlockSize { identifier, value } => identifier.span().join_span(value),
            TableOption::MaxRows { identifier, value } => identifier.span().join_span(value),
            TableOption::MinRows { identifier, value } => identifier.span().join_span(value),
            TableOption::Password { identifier, value } => identifier.span().join_span(value),
            TableOption::RowFormat { identifier, value } => identifier.span().join_span(value),
            TableOption::SecondaryEngineAttribute { identifier, value } => {
                identifier.span().join_span(value)
            }
        }
    }
}

/// Definition in create table
#[derive(Clone, Debug)]
pub enum CreateDefinition<'a> {
    ColumnDefinition {
        /// Name of column
        identifier: Identifier<'a>,
        /// Datatype and options for column
        data_type: DataType<'a>,
    },
    ConstraintDefinition {
        span: Span,
        identifier: Identifier<'a>,
    },
}

impl<'a> Spanned for CreateDefinition<'a> {
    fn span(&self) -> Span {
        match &self {
            CreateDefinition::ColumnDefinition {
                identifier,
                data_type,
            } => identifier.span().join_span(data_type),
            CreateDefinition::ConstraintDefinition { span, identifier } => {
                span.join_span(identifier)
            }
        }
    }
}

/// Special algorithm used for table creation
#[derive(Clone, Debug)]
pub enum CreateAlgorithm {
    Undefined(Span),
    Merge(Span),
    TempTable(Span),
}
impl<'a> Spanned for CreateAlgorithm {
    fn span(&self) -> Span {
        match &self {
            CreateAlgorithm::Undefined(s) => s.span(),
            CreateAlgorithm::Merge(s) => s.span(),
            CreateAlgorithm::TempTable(s) => s.span(),
        }
    }
}

/// Options for create statement
#[derive(Clone, Debug)]
pub enum CreateOption<'a> {
    OrReplace(Span),
    Temporary(Span),
    Unique(Span),
    Algorithm(Span, CreateAlgorithm),
    Definer {
        definer_span: Span,
        user: Identifier<'a>,
        host: Identifier<'a>,
    },
    SqlSecurityDefiner(Span, Span),
    SqlSecurityUser(Span, Span),
}
impl<'a> Spanned for CreateOption<'a> {
    fn span(&self) -> Span {
        match &self {
            CreateOption::OrReplace(v) => v.span(),
            CreateOption::Temporary(v) => v.span(),
            CreateOption::Algorithm(s, a) => s.join_span(a),
            CreateOption::Definer {
                definer_span,
                user,
                host,
            } => definer_span.join_span(user).join_span(host),
            CreateOption::SqlSecurityDefiner(a, b) => a.join_span(b),
            CreateOption::SqlSecurityUser(a, b) => a.join_span(b),
            CreateOption::Unique(v) => v.span(),
        }
    }
}

/// Represent a create table statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, CreateTable, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "CREATE TABLE `parts` (
///         `id` int(11) NOT NULL COMMENT 'THIS IS THE ID FIELD',
///         `hash` varchar(64) COLLATE utf8_bin NOT NULL,
///         `destination` varchar(64) COLLATE utf8_bin NOT NULL,
///         `part` varchar(64) COLLATE utf8_bin NOT NULL,
///         `success` tinyint(1) NOT NULL
///     ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;";
///
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let create: CreateTable = match stmts.pop() {
///     Some(Statement::CreateTable(c)) => c,
///     _ => panic!("We should get an create table statement")
/// };
///
/// assert!(create.identifier.as_str() == "parts");
/// println!("{:#?}", create.create_definitions)
/// ```

#[derive(Clone, Debug)]
pub struct CreateTable<'a> {
    /// Span of "CREATE"
    pub create_span: Span,
    /// Options specified after "CREATE"
    pub create_options: Vec<CreateOption<'a>>,
    /// Span of "TABLE"
    pub table_span: Span,
    /// Name of the table
    pub identifier: Identifier<'a>,
    /// Span of "IF NOT EXISTS" if specified
    pub if_not_exists: Option<Span>,
    /// Definitions of table members
    pub create_definitions: Vec<CreateDefinition<'a>>,
    /// Options specified after the table creation
    pub options: Vec<TableOption<'a>>,
}

impl<'a> Spanned for CreateTable<'a> {
    fn span(&self) -> Span {
        self.create_span
            .join_span(&self.create_options)
            .join_span(&self.table_span)
            .join_span(&self.identifier)
            .join_span(&self.if_not_exists)
            .join_span(&self.create_definitions)
            .join_span(&self.options)
    }
}

/// Represent a create view statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, CreateView, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "CREATE ALGORITHM=UNDEFINED DEFINER=`phpmyadmin`@`localhost` SQL SECURITY DEFINER
///    VIEW `v1`
///    AS SELECT
///         `t1`.`id` AS `id`,
///         `t1`.`c1` AS `c1`,
///         (SELECT `t2`.`c2` FROM `t2` WHERE `t2`.`id` = `t1`.`c3`) AS `c2`
///         FROM `t1` WHERE `t1`.`deleted` IS NULL;";
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert!(issues.is_empty());
/// #
/// let create: CreateView = match stmts.pop() {
///     Some(Statement::CreateView(c)) => c,
///     _ => panic!("We should get an create view statement")
/// };
///
/// assert!(create.name.as_str() == "v1");
/// println!("{:#?}", create.select)
/// ```

#[derive(Clone, Debug)]
pub struct CreateView<'a> {
    /// Span of "CREATE"
    pub create_span: Span,
    /// Options after "CREATE"
    pub create_options: Vec<CreateOption<'a>>,
    /// Span of "VIEW"
    pub view_span: Span,
    /// Span of "IF NOT EXISTS" if specified
    pub if_not_exists: Option<Span>,
    /// Name of the created view
    pub name: Identifier<'a>,
    /// Span of "AS"
    pub as_span: Span,
    /// The select statement following "AS"
    pub select: Select<'a>,
}

impl<'a> Spanned for CreateView<'a> {
    fn span(&self) -> Span {
        self.create_span
            .join_span(&self.create_options)
            .join_span(&self.view_span)
            .join_span(&self.if_not_exists)
            .join_span(&self.name)
            .join_span(&self.as_span)
            .join_span(&self.select)
    }
}

pub(crate) fn parse_create_constraint_definition<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<CreateDefinition<'a>, ParseError> {
    let span = parser.consume_keyword(Keyword::CONSTRAINT)?;
    let identifier = parser.consume_plain_identifier()?;
    parser.consume_keywords(&[Keyword::FOREIGN, Keyword::KEY])?;
    parser.consume_token(Token::LParen)?;
    parser.consume_plain_identifier()?;
    while parser.skip_token(Token::Comma).is_some() {
        parser.consume_plain_identifier()?;
    }
    parser.consume_token(Token::RParen)?;
    parser.consume_keyword(Keyword::REFERENCES)?;
    parser.consume_plain_identifier()?;
    parser.consume_token(Token::LParen)?;
    parser.consume_plain_identifier()?;
    while parser.skip_token(Token::Comma).is_some() {
        parser.consume_plain_identifier()?;
    }

    parser.consume_token(Token::RParen)?;
    if let Some(_) = parser.skip_keyword(Keyword::ON) {
        parser.consume_keyword(Keyword::DELETE)?;
        match parser.token {
            Token::Ident(_, Keyword::CASCADE) => {
                parser.consume_keyword(Keyword::CASCADE)?;
            }
            Token::Ident(_, Keyword::DELETE) => {
                parser.consume_keyword(Keyword::DELETE)?;
            }
            Token::Ident(_, Keyword::RESTRICT) => {
                parser.consume_keyword(Keyword::RESTRICT)?;
            }
            Token::Ident(_, Keyword::SET) => {
                parser.consume_keywords(&[Keyword::SET, Keyword::NULL])?;
            }
            _ => parser.expected_failure("CASCADE, DELETE OR SET NULL")?,
        }
    }
    Ok(CreateDefinition::ConstraintDefinition { span, identifier })
}

pub(crate) fn parse_create_definition<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<CreateDefinition<'a>, ParseError> {
    match &parser.token {
        Token::Ident(_, Keyword::CONSTRAINT) => parse_create_constraint_definition(parser),
        Token::Ident(_, _) => Ok(CreateDefinition::ColumnDefinition {
            identifier: parser.consume_plain_identifier()?,
            data_type: parse_data_type(parser, false)?,
        }),
        _ => parser.expected_failure("identifier"),
    }
}

fn parse_create_view<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let view_span = parser.consume_keyword(Keyword::VIEW)?;

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

/// Characteristic of a function
#[derive(Clone, Debug)]
pub enum FunctionCharacteristic<'a> {
    LanguageSql(Span),
    LanguagePlpgsql(Span),
    NotDeterministic(Span),
    Deterministic(Span),
    ContainsSql(Span),
    NoSql(Span),
    ReadsSqlData(Span),
    ModifiesSqlData(Span),
    SqlSecurityDefiner(Span),
    SqlSecurityUser(Span),
    Comment(SString<'a>),
}

impl<'a> Spanned for FunctionCharacteristic<'a> {
    fn span(&self) -> Span {
        match &self {
            FunctionCharacteristic::LanguageSql(v) => v.span(),
            FunctionCharacteristic::NotDeterministic(v) => v.span(),
            FunctionCharacteristic::Deterministic(v) => v.span(),
            FunctionCharacteristic::ContainsSql(v) => v.span(),
            FunctionCharacteristic::NoSql(v) => v.span(),
            FunctionCharacteristic::ReadsSqlData(v) => v.span(),
            FunctionCharacteristic::ModifiesSqlData(v) => v.span(),
            FunctionCharacteristic::SqlSecurityDefiner(v) => v.span(),
            FunctionCharacteristic::SqlSecurityUser(v) => v.span(),
            FunctionCharacteristic::Comment(v) => v.span(),
            FunctionCharacteristic::LanguagePlpgsql(v) => v.span(),
        }
    }
}

/// Direction of a function argument
#[derive(Clone, Debug)]
pub enum FunctionParamDirection {
    In(Span),
    Out(Span),
    InOut(Span),
}

impl Spanned for FunctionParamDirection {
    fn span(&self) -> Span {
        match &self {
            FunctionParamDirection::In(v) => v.span(),
            FunctionParamDirection::Out(v) => v.span(),
            FunctionParamDirection::InOut(v) => v.span(),
        }
    }
}

/// Representation of Create Function Statement
///
/// This is not fully implemented yet
///
/// ```ignore
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, CreateFunction, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "DELIMITER $$
/// CREATE FUNCTION add_func3(IN a INT, IN b INT, OUT c INT) RETURNS INT
/// BEGIN
///     SET c = 100;
///     RETURN a + b;
/// END;
/// $$
/// DELIMITER ;";
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// assert!(issues.is_empty());
/// #
/// let create: CreateFunction = match stmts.pop() {
///     Some(Statement::CreateFunction(c)) => c,
///     _ => panic!("We should get an create function statement")
/// };
///
/// assert!(create.name.as_str() == "add_func3");
/// println!("{:#?}", create.return_)
/// ```
#[derive(Clone, Debug)]
pub struct CreateFunction<'a> {
    /// Span of "CREATE"
    pub create_span: Span,
    /// Options after "CREATE"
    pub create_options: Vec<CreateOption<'a>>,
    /// Span of "FUNCTION"
    pub function_span: Span,
    /// Span of "IF NOT EXISTS" if specified
    pub if_not_exists: Option<Span>,
    /// Name o created function
    pub name: Identifier<'a>,
    /// Names and types of function arguments
    pub params: Vec<(Option<FunctionParamDirection>, Identifier<'a>, DataType<'a>)>,
    /// Span of "RETURNS"
    pub returns_span: Span,
    /// Type of return value
    pub return_type: DataType<'a>,
    /// Characteristics of created function
    pub characteristics: Vec<FunctionCharacteristic<'a>>,
    /// Statement computing return value
    pub return_: Option<Box<Statement<'a>>>,
}

impl<'a> Spanned for CreateFunction<'a> {
    fn span(&self) -> Span {
        self.create_span
            .join_span(&self.create_options)
            .join_span(&self.function_span)
            .join_span(&self.if_not_exists)
            .join_span(&self.name)
            .join_span(&self.return_type)
            .join_span(&self.characteristics)
            .join_span(&self.return_)
    }
}

fn parse_create_function<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
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
            let direction = match &parser.token {
                Token::Ident(_, Keyword::IN) => {
                    let in_ = parser.consume_keyword(Keyword::IN)?;
                    if let Some(out) = parser.skip_keyword(Keyword::OUT) {
                        Some(FunctionParamDirection::InOut(in_.join_span(&out)))
                    } else {
                        Some(FunctionParamDirection::In(in_))
                    }
                }
                Token::Ident(_, Keyword::OUT) => Some(FunctionParamDirection::Out(
                    parser.consume_keyword(Keyword::OUT)?,
                )),
                Token::Ident(_, Keyword::INOUT) => Some(FunctionParamDirection::InOut(
                    parser.consume_keyword(Keyword::INOUT)?,
                )),
                _ => None,
            };

            if parser.options.dialect.is_maria() && direction.is_none() {
                parser.expected_error("'IN', 'OUT' or 'INOUT'");
            }

            let name = parser.consume_plain_identifier()?;
            let type_ = parse_data_type(parser, false)?;
            params.push((direction, name, type_));
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
        Ok(())
    })?;
    parser.consume_token(Token::RParen)?;
    let returns_span = parser.consume_keyword(Keyword::RETURNS)?;
    let return_type = parse_data_type(parser, true)?;
    if parser.options.dialect.is_postgresql() {
        if let Some(_) = parser.skip_keyword(Keyword::AS) {
            parser.consume_token(Token::DoubleDollar)?;
            loop {
                match &parser.token {
                    Token::Eof | Token::DoubleDollar => {
                        parser.consume_token(Token::DoubleDollar)?;
                        break;
                    }
                    _ => {
                        parser.consume();
                    }
                }
            }
        }
    }

    let mut characteristics = Vec::new();
    loop {
        let f = match &parser.token {
            Token::Ident(_, Keyword::LANGUAGE) => {
                let lg = parser.consume();
                match &parser.token {
                    Token::Ident(_, Keyword::SQL) => {
                        FunctionCharacteristic::LanguageSql(lg.join_span(&parser.consume()))
                    }
                    Token::Ident(_, Keyword::PLPGSQL) => {
                        FunctionCharacteristic::LanguagePlpgsql(lg.join_span(&parser.consume()))
                    }
                    _ => parser.expected_failure("language name")?,
                }
            }
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

    let return_ = if parser.options.dialect.is_maria() {
        match parse_statement(parser)? {
            Some(v) => Some(Box::new(v)),
            None => parser.expected_failure("statement")?,
        }
    } else {
        None
    };

    Ok(Statement::CreateFunction(CreateFunction {
        create_span,
        create_options,
        function_span,
        if_not_exists,
        name,
        params,
        return_type,
        characteristics,
        return_,
        returns_span,
    }))
}

/// When to fire the trigger
#[derive(Clone, Debug)]

pub enum TriggerTime {
    Before(Span),
    After(Span),
}

impl Spanned for TriggerTime {
    fn span(&self) -> Span {
        match &self {
            TriggerTime::Before(v) => v.span(),
            TriggerTime::After(v) => v.span(),
        }
    }
}

/// On what event to fire the trigger
#[derive(Clone, Debug)]
pub enum TriggerEvent {
    Update(Span),
    Insert(Span),
    Delete(Span),
}

impl Spanned for TriggerEvent {
    fn span(&self) -> Span {
        match &self {
            TriggerEvent::Update(v) => v.span(),
            TriggerEvent::Insert(v) => v.span(),
            TriggerEvent::Delete(v) => v.span(),
        }
    }
}

/// Represent a create trigger statement
/// ```
/// # use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statements, CreateTrigger, Statement};
/// # let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
/// # let mut issues = Vec::new();
/// #
/// let sql = "DROP TRIGGER IF EXISTS `my_trigger`;
/// DELIMITER $$
/// CREATE TRIGGER `my_trigger` AFTER DELETE ON `things` FOR EACH ROW BEGIN
///     IF OLD.`value` IS NOT NULL THEN
///         UPDATE `t2` AS `j`
///             SET
///             `j`.`total_items` = `total_items` - 1
///             WHERE `j`.`id`=OLD.`value` AND NOT `j`.`frozen`;
///         END IF;
///     INSERT INTO `updated_things` (`thing`) VALUES (OLD.`id`);
/// END
/// $$
/// DELIMITER ;";
/// let mut stmts = parse_statements(sql, &mut issues, &options);
///
/// # assert_eq!(&issues, &[]);
/// #
/// let create: CreateTrigger = match stmts.pop() {
///     Some(Statement::CreateTrigger(c)) => c,
///     _ => panic!("We should get an create trigger statement")
/// };
///
/// assert!(create.name.as_str() == "my_trigger");
/// println!("{:#?}", create.statement)
/// ```
#[derive(Clone, Debug)]
pub struct CreateTrigger<'a> {
    /// Span of "CREATE"
    pub create_span: Span,
    /// Options after "CREATE"
    pub create_options: Vec<CreateOption<'a>>,
    /// Span of "TRIGGER"
    pub trigger_span: Span,
    /// Span of "IF NOT EXISTS" if specified
    pub if_not_exists: Option<Span>,
    /// Name of the created trigger
    pub name: Identifier<'a>,
    /// Should the trigger be fired before or after the event
    pub trigger_time: TriggerTime,
    /// What event should the trigger be fired on
    pub trigger_event: TriggerEvent,
    /// Span of "ON"
    pub on_span: Span,
    /// Name of table to create the trigger on
    pub table: Identifier<'a>,
    /// Span of "FOR EACH ROW"
    pub for_each_row_span: Span,
    /// Statement to execute
    pub statement: Box<Statement<'a>>,
}

impl<'a> Spanned for CreateTrigger<'a> {
    fn span(&self) -> Span {
        self.create_span
            .join_span(&self.create_options)
            .join_span(&self.trigger_span)
            .join_span(&self.if_not_exists)
            .join_span(&self.name)
            .join_span(&self.trigger_time)
            .join_span(&self.trigger_event)
            .join_span(&self.on_span)
            .join_span(&self.table)
            .join_span(&self.for_each_row_span)
            .join_span(&self.statement)
    }
}

fn parse_create_trigger<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let trigger_span = parser.consume_keyword(Keyword::TRIGGER)?;

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

    let for_each_row_span =
        parser.consume_keywords(&[Keyword::FOR, Keyword::EACH, Keyword::ROW])?;

    // TODO [{ FOLLOWS | PRECEDES } other_trigger_name ]

    let old = core::mem::replace(&mut parser.permit_compound_statements, true);
    let statement = match parse_statement(parser)? {
        Some(v) => v,
        None => parser.expected_failure("statement")?,
    };
    parser.permit_compound_statements = old;

    Ok(Statement::CreateTrigger(CreateTrigger {
        create_span,
        create_options,
        trigger_span,
        if_not_exists,
        name,
        trigger_time,
        trigger_event,
        on_span,
        table,
        for_each_row_span,
        statement: Box::new(statement),
    }))
}

#[derive(Clone, Debug)]
pub struct CreateTypeEnum<'a> {
    /// Span of "CREATE"
    pub create_span: Span,
    /// Options after "CREATE"
    pub create_options: Vec<CreateOption<'a>>,
    /// Span of "TYPE"
    pub type_span: Span,
    /// Name of the created type
    pub name: Identifier<'a>,
    /// Span of "AS ENUM"
    pub as_enum_span: Span,
    /// Enum values
    pub values: Vec<SString<'a>>,
}

impl<'a> Spanned for CreateTypeEnum<'a> {
    fn span(&self) -> Span {
        self.create_span
            .join_span(&self.create_options)
            .join_span(&self.type_span)
            .join_span(&self.name)
            .join_span(&self.as_enum_span)
            .join_span(&self.values)
    }
}

fn parse_create_type<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let type_span = parser.consume_keyword(Keyword::TYPE)?;
    if !parser.options.dialect.is_postgresql() {
        parser.issues.push(Issue::err(
            "CREATE TYPE only supported by postgresql",
            &type_span,
        ));
    }
    let name = parser.consume_plain_identifier()?;
    let as_enum_span = parser.consume_keywords(&[Keyword::AS, Keyword::ENUM])?;
    parser.consume_token(Token::LParen)?;
    let mut values = Vec::new();
    loop {
        parser.recovered(
            "')' or ','",
            &|t| matches!(t, Token::RParen | Token::Comma),
            |parser| {
                values.push(parser.consume_string()?);
                Ok(())
            },
        )?;
        if matches!(parser.token, Token::RParen) {
            break;
        }
        parser.consume_token(Token::Comma)?;
    }
    parser.consume_token(Token::RParen)?;
    Ok(Statement::CreateTypeEnum(CreateTypeEnum {
        create_span,
        create_options,
        type_span,
        name,
        as_enum_span,
        values,
    }))
}

#[derive(Clone, Debug)]
pub enum CreateIndexOption {
    UsingGist(Span),
}

impl Spanned for CreateIndexOption {
    fn span(&self) -> Span {
        match self {
            CreateIndexOption::UsingGist(s) => s.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CreateIndex<'a> {
    pub create_span: Span,
    pub create_options: Vec<CreateOption<'a>>,
    pub index_span: Span,
    pub index_name: Identifier<'a>,
    pub if_not_exists: Option<Span>,
    pub on_span: Span,
    pub table_name: Identifier<'a>,
    pub index_options: Vec<CreateIndexOption>,
    pub l_paren_span: Span,
    pub column_names: Vec<Identifier<'a>>,
    pub r_paren_span: Span,
    pub where_: Option<(Span, Expression<'a>)>,
}

impl<'a> Spanned for CreateIndex<'a> {
    fn span(&self) -> Span {
        self.create_span
            .join_span(&self.create_options)
            .join_span(&self.index_span)
            .join_span(&self.index_name)
            .join_span(&self.on_span)
            .join_span(&self.table_name)
            .join_span(&self.index_options)
            .join_span(&self.l_paren_span)
            .join_span(&self.column_names)
            .join_span(&self.r_paren_span)
            .join_span(&self.where_)
    }
}

fn parse_create_index<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let index_span = parser.consume_keyword(Keyword::INDEX)?;
    let if_not_exists = if let Some(s) = parser.skip_keyword(Keyword::IF) {
        Some(s.join_span(&parser.consume_keywords(&[Keyword::NOT, Keyword::EXISTS])?))
    } else {
        None
    };
    let index_name = parser.consume_plain_identifier()?;
    let on_span = parser.consume_keyword(Keyword::ON)?;
    let table_name = parser.consume_plain_identifier()?;
    let mut index_options = Vec::new();
    if let Some(using_span) = parser.skip_keyword(Keyword::USING) {
        let gist_span = parser.consume_keyword(Keyword::GIST)?;
        index_options.push(CreateIndexOption::UsingGist(
            gist_span.join_span(&using_span),
        ));
    }
    let l_paren_span = parser.consume_token(Token::LParen)?;
    let mut column_names = Vec::new();
    column_names.push(parser.consume_plain_identifier()?);
    while parser.skip_token(Token::Comma).is_some() {
        column_names.push(parser.consume_plain_identifier()?);
    }
    let r_paren_span = parser.consume_token(Token::RParen)?;

    let mut where_ = None;
    if let Some(where_span) = parser.skip_keyword(Keyword::WHERE) {
        let where_expr = parse_expression(parser, false)?;
        if parser.options.dialect.is_maria() {
            parser.issues.push(Issue::err(
                "Partial indexes not supported",
                &where_span.join_span(&where_expr),
            ));
        }
        where_ = Some((where_span, where_expr));
    }

    Ok(Statement::CreateIndex(CreateIndex {
        create_span,
        create_options,
        index_span,
        index_name,
        if_not_exists,
        on_span,
        table_name,
        index_options,
        l_paren_span,
        column_names,
        r_paren_span,
        where_,
    }))
}

fn parse_create_table<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
    create_span: Span,
    create_options: Vec<CreateOption<'a>>,
) -> Result<Statement<'a>, ParseError> {
    let table_span = parser.consume_keyword(Keyword::TABLE)?;

    let mut identifier = Identifier::new("", 0..0);
    let mut if_not_exists = None;

    parser.recovered("'('", &|t| t == &Token::LParen, |parser| {
        if let Some(if_) = parser.skip_keyword(Keyword::IF) {
            if_not_exists = Some(
                if_.start
                    ..parser
                        .consume_keywords(&[Keyword::NOT, Keyword::EXISTS])?
                        .end,
            );
        }
        identifier = parser.consume_plain_identifier()?;
        Ok(())
    })?;

    parser.consume_token(Token::LParen)?;

    let mut create_definitions = Vec::new();
    if !matches!(parser.token, Token::RParen) {
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

pub(crate) fn parse_create<'a, 'b>(
    parser: &mut Parser<'a, 'b>,
) -> Result<Statement<'a>, ParseError> {
    let create_span = parser.span.clone();
    parser.consume_keyword(Keyword::CREATE)?;

    let mut create_options = Vec::new();
    const CREATABLE: &str = "'TABLE' | 'VIEW' | 'TRIGGER' | 'FUNCTION' | 'INDEX' | 'TYPE'";

    parser.recovered(
        CREATABLE,
        &|t| {
            matches!(
                t,
                Token::Ident(
                    _,
                    Keyword::TABLE
                        | Keyword::VIEW
                        | Keyword::TRIGGER
                        | Keyword::FUNCTION
                        | Keyword::INDEX
                        | Keyword::TYPE
                )
            )
        },
        |parser| {
            loop {
                let v = match &parser.token {
                    Token::Ident(_, Keyword::OR) => CreateOption::OrReplace(
                        parser.consume_keywords(&[Keyword::OR, Keyword::REPLACE])?,
                    ),
                    Token::Ident(_, Keyword::TEMPORARY) => {
                        CreateOption::Temporary(parser.consume_keyword(Keyword::TEMPORARY)?)
                    }
                    Token::Ident(_, Keyword::UNIQUE) => {
                        CreateOption::Unique(parser.consume_keyword(Keyword::UNIQUE)?)
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
                        let sql_security =
                            parser.consume_keywords(&[Keyword::SQL, Keyword::SECURITY])?;
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
        Token::Ident(_, Keyword::INDEX) => parse_create_index(parser, create_span, create_options),
        Token::Ident(_, Keyword::TABLE) => parse_create_table(parser, create_span, create_options),
        Token::Ident(_, Keyword::VIEW) => parse_create_view(parser, create_span, create_options),
        Token::Ident(_, Keyword::FUNCTION) => {
            parse_create_function(parser, create_span, create_options)
        }
        Token::Ident(_, Keyword::TRIGGER) => {
            parse_create_trigger(parser, create_span, create_options)
        }
        Token::Ident(_, Keyword::TYPE) => parse_create_type(parser, create_span, create_options),
        _ => parser.expected_failure(CREATABLE),
    }
}
