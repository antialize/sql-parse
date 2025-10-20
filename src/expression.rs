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
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser},
    select::{parse_select, OrderFlag},
    span::OptSpanned,
    statement::parse_compound_query,
    DataType, Identifier, SString, Span, Spanned, Statement,
};
use alloc::string::ToString;
use alloc::vec;
use alloc::{boxed::Box, vec::Vec};

/// Function to execute
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function<'a> {
    Abs,
    Acos,
    AddDate,
    AddMonths,
    AddTime,
    Ascii,
    Asin,
    Atan,
    Atan2,
    Bin,
    BitLength,
    Ceil,
    CharacterLength,
    Chr,
    Concat,
    ConcatWs,
    Conv,
    ConvertTz,
    Cos,
    Cot,
    Crc32,
    Crc32c,
    CurDate,
    CurrentTimestamp,
    CurTime,
    Date,
    DateDiff,
    DateFormat,
    DateSub,
    Datetime,
    DayName,
    DayOfMonth,
    DayOfWeek,
    DayOfYear,
    Degrees,
    Elt,
    Exists,
    Exp,
    ExportSet,
    ExtractValue,
    Field,
    FindInSet,
    Floor,
    Format,
    FromBase64,
    FromDays,
    FromUnixTime,
    Greatest,
    Hex,
    Hour,
    If,
    IfNull,
    Insert,
    InStr,
    JsonArray,
    JsonArrayAgg,
    JsonArrayAppend,
    JsonArrayInsert,
    JsonArrayIntersect,
    JsonCompact,
    JsonContains,
    JsonContainsPath,
    JsonDepth,
    JsonDetailed,
    JsonEquals,
    JsonExists,
    JsonExtract,
    JsonInsert,
    JsonKeys,
    JsonLength,
    JsonLoose,
    JsonMerge,
    JsonMergePath,
    JsonMergePerserve,
    JsonNormalize,
    JsonObject,
    JsonObjectAgg,
    JsonObjectFilterKeys,
    JsonObjectToArray,
    JsonOverlaps,
    JsonPretty,
    JsonQuery,
    JsonQuote,
    JsonRemove,
    JsonReplace,
    JsonSchemaValid,
    JsonSearch,
    JsonSet,
    JsonTable,
    JsonType,
    JsonUnquote,
    JsonValid,
    JsonValue,
    Lag,
    LastDay,
    LCase,
    Lead,
    Least,
    Left,
    Length,
    LengthB,
    Ln,
    LoadFile,
    Locate,
    Log,
    Log10,
    Log2,
    Lower,
    LPad,
    LTrim,
    MakeDate,
    MakeSet,
    MakeTime,
    Max,
    MicroSecond,
    Mid,
    Min,
    Minute,
    Month,
    MonthName,
    NaturalSortkey,
    Now,
    NullIf,
    NVL2,
    Oct,
    OctetLength,
    Ord,
    PeriodAdd,
    PeriodDiff,
    Pi,
    Position,
    Pow,
    Quarter,
    Quote,
    Radians,
    Rand,
    Repeat,
    Replace,
    Reverse,
    Right,
    Round,
    RPad,
    RTrim,
    Second,
    SecToTime,
    SFormat,
    Sign,
    Sin,
    SoundEx,
    Space,
    Sqrt,
    StartsWith,
    StrCmp,
    Strftime,
    StrToDate,
    SubStr,
    SubStringIndex,
    SubTime,
    Sum,
    SysDate,
    Tan,
    Time,
    TimeDiff,
    TimeFormat,
    Timestamp,
    TimeToSec,
    ToBase64,
    ToChar,
    ToDays,
    ToSeconds,
    Truncate,
    UCase,
    UncompressedLength,
    UnHex,
    UnixTimestamp,
    Unknown,
    UpdateXml,
    Upper,
    UtcDate,
    UtcTime,
    UtcTimeStamp,
    Value,
    Week,
    Weekday,
    WeekOfYear,
    Year,
    YearWeek,
    Other(&'a str),
}

/// Function to execute
#[derive(Debug, Clone)]
pub enum Variable<'a> {
    TimeZone,
    Other(&'a str),
}

/// Binary operator to apply
#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Or,
    Xor,
    And,
    Eq,
    NullSafeEq,
    GtEq,
    Gt,
    LtEq,
    Lt,
    Neq,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    BitOr,
    BitXor,
    Add,
    Subtract,
    Divide,
    Div,
    Mod,
    Mult,
    Like,
    NotLike,
}

/// Type of is expression
#[derive(Debug, Clone, Copy)]
pub enum Is {
    Null,
    NotNull,
    True,
    NotTrue,
    False,
    NotFalse,
    Unknown,
    NotUnknown,
}

/// Unary operator to apply
#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Binary,
    Collate,
    LogicalNot,
    Minus,
    Not,
}

/// Part of a full identifier
#[derive(Debug, Clone)]
pub enum IdentifierPart<'a> {
    Name(Identifier<'a>),
    Star(Span),
}

impl<'a> Spanned for IdentifierPart<'a> {
    fn span(&self) -> Span {
        match &self {
            IdentifierPart::Name(v) => v.span(),
            IdentifierPart::Star(v) => v.span(),
        }
    }
}

/// When part of CASE
#[derive(Debug, Clone)]
pub struct When<'a> {
    /// Span of WHEN
    pub when_span: Span,
    /// When to return then
    pub when: Expression<'a>,
    /// Span of THEN
    pub then_span: Span,
    /// What to return when when applyes
    pub then: Expression<'a>,
}

impl<'a> Spanned for When<'a> {
    fn span(&self) -> Span {
        self.when_span
            .join_span(&self.when)
            .join_span(&self.then_span)
            .join_span(&self.then)
    }
}

/// When part of CASE
#[derive(Debug, Clone)]
pub struct WindowSpec<'a> {
    /// Span of "ORDER BY" and list of order expression and directions, if specified
    pub order_by: (Span, Vec<(Expression<'a>, OrderFlag)>),
}

impl<'a> Spanned for WindowSpec<'a> {
    fn span(&self) -> Span {
        self.order_by.span()
    }
}

/// Units of time
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimeUnit {
    /// Microseconds
    Microsecond,
    /// Seconds
    Second,
    /// Minutes
    Minute,
    /// Hours
    Hour,
    /// Days
    Day,
    /// Weeks
    Week,
    /// Months
    Month,
    /// Quarters
    Quarter,
    /// Years
    Year,
    /// Seconds.Microseconds
    SecondMicrosecond,
    /// Minutes.Seconds.Microseconds
    MinuteMicrosecond,
    /// Minutes.Seconds
    MinuteSecond,
    /// Hours.Minutes.Seconds.Microseconds
    HourMicrosecond,
    /// Hours.Minutes.Seconds
    HourSecond,
    /// Hours.Minutes
    HourMinute,
    /// Days Hours.Minutes.Seconds.Microseconds
    DayMicrosecond,
    /// Days Hours.Minutes.Seconds
    DaySecond,
    /// Days Hours.Minutes
    DayMinute,
    /// Days Hours
    DayHour,
    /// Years-Months
    YearMonth,
}

fn parse_time_unit(t: &Token<'_>) -> Option<TimeUnit> {
    match t {
        Token::Ident(_, Keyword::MICROSECOND) => Some(TimeUnit::Microsecond),
        Token::Ident(_, Keyword::SECOND) => Some(TimeUnit::Second),
        Token::Ident(_, Keyword::MINUTE) => Some(TimeUnit::Minute),
        Token::Ident(_, Keyword::HOUR) => Some(TimeUnit::Hour),
        Token::Ident(_, Keyword::DAY) => Some(TimeUnit::Day),
        Token::Ident(_, Keyword::WEEK) => Some(TimeUnit::Week),
        Token::Ident(_, Keyword::MONTH) => Some(TimeUnit::Month),
        Token::Ident(_, Keyword::QUARTER) => Some(TimeUnit::Quarter),
        Token::Ident(_, Keyword::YEAR) => Some(TimeUnit::Year),
        Token::Ident(_, Keyword::SECOND_MICROSECOND) => Some(TimeUnit::SecondMicrosecond),
        Token::Ident(_, Keyword::MINUTE_MICROSECOND) => Some(TimeUnit::MinuteMicrosecond),
        Token::Ident(_, Keyword::MINUTE_SECOND) => Some(TimeUnit::MinuteSecond),
        Token::Ident(_, Keyword::HOUR_MICROSECOND) => Some(TimeUnit::HourMicrosecond),
        Token::Ident(_, Keyword::HOUR_SECOND) => Some(TimeUnit::HourSecond),
        Token::Ident(_, Keyword::HOUR_MINUTE) => Some(TimeUnit::HourMinute),
        Token::Ident(_, Keyword::DAY_MICROSECOND) => Some(TimeUnit::DayMicrosecond),
        Token::Ident(_, Keyword::DAY_SECOND) => Some(TimeUnit::DaySecond),
        Token::Ident(_, Keyword::DAY_MINUTE) => Some(TimeUnit::DayMinute),
        Token::Ident(_, Keyword::DAY_HOUR) => Some(TimeUnit::DayHour),
        Token::Ident(_, Keyword::YEAR_MONTH) => Some(TimeUnit::YearMonth),
        _ => None,
    }
}

/// Representation of an expression
#[derive(Debug, Clone)]
pub enum Expression<'a> {
    /// Expression with binary operator
    Binary {
        /// The operator to apply
        op: BinaryOperator,
        /// The span of the operator
        op_span: Span,
        /// Expression on the left hand side
        lhs: Box<Expression<'a>>,
        /// Expression on the right hand side
        rhs: Box<Expression<'a>>,
    },
    /// Expression with a unary (prefix) operator
    Unary {
        /// The operator to apply
        op: UnaryOperator,
        /// The span of the operator
        op_span: Span,
        /// The expression on the right hand side
        operand: Box<Expression<'a>>,
    },
    /// Subquery expression
    Subquery(Box<Statement<'a>>),
    /// Literal NULL expression
    Null(Span),
    /// Literal bool expression "TRUE" or "FALSE"
    Bool(bool, Span),
    /// Literal string expression, the SString contains the represented string
    /// with escaping removed
    String(SString<'a>),
    /// Literal integer expression
    Integer((u64, Span)),
    /// Literal _LIST_
    ListHack((usize, Span)),
    /// Literal floating point expression
    Float((f64, Span)),
    /// Function call expression,
    Function(Function<'a>, Vec<Expression<'a>>, Span),
    /// A window function call expression
    WindowFunction {
        function: Function<'a>,
        args: Vec<Expression<'a>>,
        function_span: Span,
        over_span: Span,
        window_spec: WindowSpec<'a>,
    },
    /// Identifier pointing to column
    Identifier(Vec<IdentifierPart<'a>>),
    /// Time Interval
    Interval {
        /// Span of "INTERVAL"
        interval_span: Span,
        /// Time internal
        time_interval: (Vec<i64>, Span),
        /// Unit of the time interval
        time_unit: (TimeUnit, Span),
    },
    /// Input argument to query, the first argument is the occurrence number of the argumnet
    Arg((usize, Span)),
    /// Exists expression
    Exists(Box<Statement<'a>>),
    Extract {
        /// Span of "EXTRACT"
        extract_span: Span,
        /// Unit of the time interval
        time_unit: (TimeUnit, Span),
        /// Span of "FROM"
        from_span: Span,
        /// Date expression
        date: Box<Expression<'a>>,
    },
    /// In expression
    In {
        /// Left hand side expression
        lhs: Box<Expression<'a>>,
        /// Right hand side expression
        rhs: Vec<Expression<'a>>,
        /// Span of "IN" or "NOT IN"
        in_span: Span,
        /// True if not in
        not_in: bool,
    },
    /// Is expression
    Is(Box<Expression<'a>>, Is, Span),
    /// Invalid expression, returned on recovery of a parse error
    Invalid(Span),
    /// Case expression
    Case {
        /// Span of "CASE"
        case_span: Span,
        /// Optional value to switch over
        value: Option<Box<Expression<'a>>>,
        /// When parts
        whens: Vec<When<'a>>,
        /// Span of "ELSE" and else value if specified
        else_: Option<(Span, Box<Expression<'a>>)>,
        /// Span of "END"
        end_span: Span,
    },
    /// Cast expression
    Cast {
        /// Span of "CAST"
        cast_span: Span,
        /// Value to cast
        expr: Box<Expression<'a>>,
        /// Span of "AS"
        as_span: Span,
        /// Type to cast to
        type_: DataType<'a>,
    },
    /// Count expression
    Count {
        /// Span of "COUNT"
        count_span: Span,
        /// Span of "DISTINCT" if specified
        distinct_span: Option<Span>,
        /// Expression to count
        expr: Box<Expression<'a>>,
    },
    /// Group contat expression
    GroupConcat {
        /// Span of "GROUP_CONCAT"
        group_concat_span: Span,
        /// Span of "DISTINCT" if specified
        distinct_span: Option<Span>,
        /// Expression to count
        expr: Box<Expression<'a>>,
    },
    /// Variable expression
    Variable {
        /// Span of "@@GLOBAL"
        global: Option<Span>,
        /// Span of "@@SESSION"
        session: Option<Span>,
        /// Span of '.'
        dot: Option<Span>,
        /// variable
        variable: Variable<'a>,
        // Span of variable
        variable_span: Span,
    },
    /// Timestampadd call
    TimestampAdd {
        timestamp_add_span: Span,
        unit: (TimeUnit, Span),
        interval: Box<Expression<'a>>,
        datetime: Box<Expression<'a>>,
    },
    /// Timestampdiff call
    TimestampDiff {
        timestamp_diff_span: Span,
        unit: (TimeUnit, Span),
        e1: Box<Expression<'a>>,
        e2: Box<Expression<'a>>,
    },
}

impl<'a> Spanned for Expression<'a> {
    fn span(&self) -> Span {
        match &self {
            Expression::Binary {
                op_span, lhs, rhs, ..
            } => op_span.join_span(lhs).join_span(rhs),
            Expression::Unary {
                op_span, operand, ..
            } => op_span.join_span(operand),
            Expression::Subquery(v) => v.span(),
            Expression::Null(v) => v.span(),
            Expression::Bool(_, v) => v.span(),
            Expression::String(v) => v.span(),
            Expression::Integer(v) => v.span(),
            Expression::Float(v) => v.span(),
            Expression::ListHack((_, s)) => s.span(),
            Expression::Function(_, b, c) => c.join_span(b),
            Expression::Identifier(v) => v.opt_span().expect("Span of identifier parts"),
            Expression::Arg(v) => v.span(),
            Expression::Exists(v) => v.span(),
            Expression::In {
                lhs, rhs, in_span, ..
            } => in_span.join_span(lhs).join_span(rhs),
            Expression::Is(a, _, b) => b.join_span(a),
            Expression::Invalid(s) => s.span(),
            Expression::Case {
                case_span,
                value,
                whens,
                else_,
                end_span,
            } => case_span
                .join_span(value)
                .join_span(whens)
                .join_span(else_)
                .join_span(end_span),
            Expression::Cast {
                cast_span,
                expr,
                as_span,
                type_,
            } => cast_span
                .join_span(expr)
                .join_span(as_span)
                .join_span(type_),
            Expression::Count {
                count_span,
                distinct_span,
                expr,
            } => count_span.join_span(distinct_span).join_span(expr),
            Expression::GroupConcat {
                group_concat_span,
                distinct_span,
                expr,
            } => group_concat_span.join_span(distinct_span).join_span(expr),
            Expression::Variable {
                global,
                session,
                dot,
                variable_span,
                variable: _,
            } => variable_span
                .join_span(global)
                .join_span(session)
                .join_span(dot),
            Expression::WindowFunction {
                function: _,
                args,
                function_span,
                over_span,
                window_spec,
            } => function_span
                .join_span(args)
                .join_span(over_span)
                .join_span(window_spec),
            Expression::Interval {
                interval_span,
                time_interval,
                time_unit,
            } => interval_span
                .join_span(&time_interval.1)
                .join_span(&time_unit.1),
            Expression::Extract {
                extract_span,
                time_unit,
                from_span,
                date,
            } => extract_span
                .join_span(&time_unit.1)
                .join_span(from_span)
                .join_span(date),
            Expression::TimestampAdd {
                timestamp_add_span: timespan_add_span,
                unit,
                interval,
                datetime,
            } => timespan_add_span
                .join_span(&unit.1)
                .join_span(interval)
                .join_span(datetime),
            Expression::TimestampDiff {
                timestamp_diff_span,
                unit,
                e1,
                e2,
            } => timestamp_diff_span
                .join_span(&unit.1)
                .join_span(e1)
                .join_span(e2),
        }
    }
}

fn parse_function<'a>(
    parser: &mut Parser<'a, '_>,
    t: Token<'a>,
    span: Span,
) -> Result<Expression<'a>, ParseError> {
    parser.consume_token(Token::LParen)?;
    let func = match &t {
        // https://mariadb.com/kb/en/string-functions/
        Token::Ident(_, Keyword::ASCII) => Function::Ascii,
        Token::Ident(_, Keyword::BIN) => Function::Bin,
        Token::Ident(_, Keyword::BIT_LENGTH) => Function::BitLength,
        Token::Ident(_, Keyword::CHAR_LENGTH) => Function::CharacterLength,
        Token::Ident(_, Keyword::CHARACTER_LENGTH) => Function::CharacterLength,
        Token::Ident(_, Keyword::CHR) => Function::Chr,
        Token::Ident(_, Keyword::CONCAT) => Function::Concat,
        Token::Ident(_, Keyword::CONCAT_WS) => Function::ConcatWs,
        Token::Ident(_, Keyword::ELT) => Function::Elt,
        Token::Ident(_, Keyword::EXPORT_SET) => Function::ExportSet,
        Token::Ident(_, Keyword::EXTRACTVALUE) => Function::ExtractValue,
        Token::Ident(_, Keyword::FIELD) => Function::Field,
        Token::Ident(_, Keyword::FIND_IN_SET) => Function::FindInSet,
        Token::Ident(_, Keyword::FORMAT) => Function::Format,
        Token::Ident(_, Keyword::FROM_BASE64) => Function::FromBase64,
        Token::Ident(_, Keyword::HEX) => Function::Hex,
        Token::Ident(_, Keyword::INSERT) => Function::Insert,
        Token::Ident(_, Keyword::INSTR) => Function::InStr,
        Token::Ident(_, Keyword::LCASE) => Function::LCase,
        Token::Ident(_, Keyword::LEFT) => Function::Left,
        Token::Ident(_, Keyword::LENGTH) => Function::Length,
        Token::Ident(_, Keyword::LENGTHB) => Function::LengthB,
        Token::Ident(_, Keyword::LOAD_FILE) => Function::LoadFile,
        Token::Ident(_, Keyword::LOCATE) => Function::Locate,
        Token::Ident(_, Keyword::LOWER) => Function::Lower,
        Token::Ident(_, Keyword::LPAD) => Function::LPad,
        Token::Ident(_, Keyword::LTRIM) => Function::LTrim,
        Token::Ident(_, Keyword::MAKE_SET) => Function::MakeSet,
        Token::Ident(_, Keyword::MID) => Function::Mid,
        Token::Ident(_, Keyword::NATURAL_SORT_KEY) => Function::NaturalSortkey,
        Token::Ident(_, Keyword::OCTET_LENGTH) => Function::OctetLength,
        Token::Ident(_, Keyword::ORD) => Function::Ord,
        Token::Ident(_, Keyword::POSITION) => Function::Position,
        Token::Ident(_, Keyword::QUOTE) => Function::Quote,
        Token::Ident(_, Keyword::REPEAT) => Function::Repeat,
        Token::Ident(_, Keyword::REPLACE) => Function::Replace,
        Token::Ident(_, Keyword::REVERSE) => Function::Reverse,
        Token::Ident(_, Keyword::RIGHT) => Function::Right,
        Token::Ident(_, Keyword::RPAD) => Function::RPad,
        Token::Ident(_, Keyword::RTRIM) => Function::RTrim,
        Token::Ident(_, Keyword::SOUNDEX) => Function::SoundEx,
        Token::Ident(_, Keyword::SPACE) => Function::Space,
        Token::Ident(_, Keyword::STRCMP) => Function::StrCmp,
        Token::Ident(_, Keyword::SUBSTR) => Function::SubStr,
        Token::Ident(_, Keyword::SUBSTRING) => Function::SubStr,
        Token::Ident(_, Keyword::SUBSTRING_INDEX) => Function::SubStringIndex,
        Token::Ident(_, Keyword::TO_BASE64) => Function::ToBase64,
        Token::Ident(_, Keyword::TO_CHAR) => Function::ToChar,
        Token::Ident(_, Keyword::UCASE) => Function::UCase,
        Token::Ident(_, Keyword::UNCOMPRESSED_LENGTH) => Function::UncompressedLength,
        Token::Ident(_, Keyword::UNHEX) => Function::UnHex,
        Token::Ident(_, Keyword::UPDATEXML) => Function::UpdateXml,
        Token::Ident(_, Keyword::UPPER) => Function::Upper,
        Token::Ident(_, Keyword::SFORMAT) => Function::SFormat,

        // TODO uncat
        Token::Ident(_, Keyword::EXISTS) => Function::Exists,
        Token::Ident(_, Keyword::MIN) => Function::Min,
        Token::Ident(_, Keyword::MAX) => Function::Max,
        Token::Ident(_, Keyword::SUM) => Function::Sum,
        Token::Ident(_, Keyword::VALUE) => Function::Value,
        Token::Ident(_, Keyword::VALUES) => Function::Value,
        Token::Ident(_, Keyword::LEAD) => Function::Lead,
        Token::Ident(_, Keyword::LAG) => Function::Lag,
        Token::Ident(_, Keyword::STARTS_WITH) => Function::StartsWith,

        //https://mariadb.com/kb/en/control-flow-functions/
        Token::Ident(_, Keyword::IFNULL) => Function::IfNull,
        Token::Ident(_, Keyword::NULLIF) => Function::NullIf,
        Token::Ident(_, Keyword::NVL) => Function::IfNull,
        Token::Ident(_, Keyword::NVL2) => Function::NVL2,
        Token::Ident(_, Keyword::IF) => Function::If,

        //https://mariadb.com/kb/en/numeric-functions/
        Token::Ident(_, Keyword::ABS) => Function::Abs,
        Token::Ident(_, Keyword::ACOS) => Function::Acos,
        Token::Ident(_, Keyword::ASIN) => Function::Asin,
        Token::Ident(_, Keyword::ATAN) => Function::Atan,
        Token::Ident(_, Keyword::ATAN2) => Function::Atan2,
        Token::Ident(_, Keyword::CEIL | Keyword::CEILING) => Function::Ceil,
        Token::Ident(_, Keyword::CONV) => Function::Conv,
        Token::Ident(_, Keyword::COS) => Function::Cos,
        Token::Ident(_, Keyword::COT) => Function::Cot,
        Token::Ident(_, Keyword::CRC32) => Function::Crc32,
        Token::Ident(_, Keyword::DEGREES) => Function::Degrees,
        Token::Ident(_, Keyword::EXP) => Function::Exp,
        Token::Ident(_, Keyword::FLOOR) => Function::Floor,
        Token::Ident(_, Keyword::GREATEST) => Function::Greatest,
        Token::Ident(_, Keyword::LN) => Function::Ln,
        Token::Ident(_, Keyword::LOG) => Function::Log,
        Token::Ident(_, Keyword::LOG10) => Function::Log10,
        Token::Ident(_, Keyword::LOG2) => Function::Log2,
        Token::Ident(_, Keyword::OCT) => Function::Oct,
        Token::Ident(_, Keyword::PI) => Function::Pi,
        Token::Ident(_, Keyword::POW | Keyword::POWER) => Function::Pow,
        Token::Ident(_, Keyword::RADIANS) => Function::Radians,
        Token::Ident(_, Keyword::RAND) => Function::Rand,
        Token::Ident(_, Keyword::ROUND) => Function::Round,
        Token::Ident(_, Keyword::SIGN) => Function::Sign,
        Token::Ident(_, Keyword::SIN) => Function::Sin,
        Token::Ident(_, Keyword::SQRT) => Function::Sqrt,
        Token::Ident(_, Keyword::TAN) => Function::Tan,
        Token::Ident(_, Keyword::TRUNCATE) => Function::Truncate,
        Token::Ident(_, Keyword::CRC32C) => Function::Crc32c,
        Token::Ident(_, Keyword::LEAST) => Function::Least,

        // https://mariadb.com/kb/en/date-time-functions/
        Token::Ident(_, Keyword::ADDDATE) => Function::AddDate,
        Token::Ident(_, Keyword::ADDTIME) => Function::AddTime,
        Token::Ident(_, Keyword::CONVERT_TZ) => Function::ConvertTz,
        Token::Ident(_, Keyword::CURDATE) => Function::CurDate,
        Token::Ident(_, Keyword::CURRENT_DATE) => Function::CurDate,
        Token::Ident(_, Keyword::CURRENT_TIME) => Function::CurTime,
        Token::Ident(_, Keyword::CURTIME) => Function::CurTime,
        Token::Ident(_, Keyword::DATE) => Function::Date,
        Token::Ident(_, Keyword::HOUR) => Function::Hour,
        Token::Ident(_, Keyword::DATEDIFF) => Function::DateDiff,
        Token::Ident(_, Keyword::DATE_ADD) => Function::AddDate,
        Token::Ident(_, Keyword::DATE_FORMAT) => Function::DateFormat,
        Token::Ident(_, Keyword::DATE_SUB) => Function::DateSub,
        Token::Ident(_, Keyword::DAY | Keyword::DAYOFMONTH) => Function::DayOfMonth,
        Token::Ident(_, Keyword::DAYNAME) => Function::DayName,
        Token::Ident(_, Keyword::DAYOFWEEK) => Function::DayOfWeek,
        Token::Ident(_, Keyword::DAYOFYEAR) => Function::DayOfYear,
        Token::Ident(_, Keyword::FROM_DAYS) => Function::FromDays,
        Token::Ident(_, Keyword::CURRENT_TIMESTAMP) => Function::CurrentTimestamp,
        Token::Ident(_, Keyword::LOCALTIME | Keyword::LOCALTIMESTAMP | Keyword::NOW) => {
            Function::Now
        }
        Token::Ident(_, Keyword::MAKEDATE) => Function::MakeDate,
        Token::Ident(_, Keyword::MAKETIME) => Function::MakeTime,
        Token::Ident(_, Keyword::MICROSECOND) => Function::MicroSecond,
        Token::Ident(_, Keyword::MINUTE) => Function::Minute,
        Token::Ident(_, Keyword::MONTH) => Function::Month,
        Token::Ident(_, Keyword::MONTHNAME) => Function::MonthName,
        Token::Ident(_, Keyword::PERIOD_ADD) => Function::PeriodAdd,
        Token::Ident(_, Keyword::PERIOD_DIFF) => Function::PeriodDiff,
        Token::Ident(_, Keyword::QUARTER) => Function::Quarter,
        Token::Ident(_, Keyword::SECOND) => Function::Second,
        Token::Ident(_, Keyword::SEC_TO_TIME) => Function::SecToTime,
        Token::Ident(_, Keyword::STR_TO_DATE) => Function::StrToDate,
        Token::Ident(_, Keyword::SUBDATE) => Function::DateSub,
        Token::Ident(_, Keyword::SUBTIME) => Function::SubTime,
        Token::Ident(_, Keyword::TIME) => Function::Time,
        Token::Ident(_, Keyword::LAST_DAY) => Function::LastDay,
        Token::Ident(_, Keyword::TIMEDIFF) => Function::TimeDiff,
        Token::Ident(_, Keyword::TIMESTAMP) => Function::Timestamp,
        Token::Ident(_, Keyword::TIME_FORMAT) => Function::TimeFormat,
        Token::Ident(_, Keyword::TIME_TO_SEC) => Function::TimeToSec,
        Token::Ident(_, Keyword::TO_DAYS) => Function::ToDays,
        Token::Ident(_, Keyword::TO_SECONDS) => Function::ToSeconds,
        Token::Ident(_, Keyword::UNIX_TIMESTAMP) => Function::UnixTimestamp,
        Token::Ident(_, Keyword::UTC_DATE) => Function::UtcDate,
        Token::Ident(_, Keyword::UTC_TIME) => Function::UtcTime,
        Token::Ident(_, Keyword::UTC_TIMESTAMP) => Function::UtcTimeStamp,
        Token::Ident(_, Keyword::WEEK) => Function::Week,
        Token::Ident(_, Keyword::WEEKDAY) => Function::Weekday,
        Token::Ident(_, Keyword::WEEKOFYEAR) => Function::WeekOfYear,
        Token::Ident(_, Keyword::ADD_MONTHS) => Function::AddMonths,
        Token::Ident(_, Keyword::FROM_UNIXTIME) => Function::FromUnixTime,
        Token::Ident(_, Keyword::YEAR) => Function::Year,
        Token::Ident(_, Keyword::YEARWEEK) => Function::YearWeek,
        Token::Ident(_, Keyword::SYSDATE) => Function::SysDate,

        // https://mariadb.com/kb/en/json-functions/
        Token::Ident(_, Keyword::JSON_ARRAY) => Function::JsonArray,
        Token::Ident(_, Keyword::JSON_ARRAYAGG) => Function::JsonArrayAgg,
        Token::Ident(_, Keyword::JSON_ARRAY_APPEND) => Function::JsonArrayAppend,
        Token::Ident(_, Keyword::JSON_ARRAY_INSERT) => Function::JsonArrayInsert,
        Token::Ident(_, Keyword::JSON_ARRAY_INTERSECT) => Function::JsonArrayIntersect,
        Token::Ident(_, Keyword::JSON_COMPACT) => Function::JsonCompact,
        Token::Ident(_, Keyword::JSON_CONTAINS) => Function::JsonContains,
        Token::Ident(_, Keyword::JSON_CONTAINS_PATH) => Function::JsonContainsPath,
        Token::Ident(_, Keyword::JSON_DEPTH) => Function::JsonDepth,
        Token::Ident(_, Keyword::JSON_DETAILED) => Function::JsonDetailed,
        Token::Ident(_, Keyword::JSON_EQUALS) => Function::JsonEquals,
        Token::Ident(_, Keyword::JSON_EXISTS) => Function::JsonExists,
        Token::Ident(_, Keyword::JSON_EXTRACT) => Function::JsonExtract,
        Token::Ident(_, Keyword::JSON_INSERT) => Function::JsonInsert,
        Token::Ident(_, Keyword::JSON_KEYS) => Function::JsonKeys,
        Token::Ident(_, Keyword::JSON_LENGTH) => Function::JsonLength,
        Token::Ident(_, Keyword::JSON_LOOSE) => Function::JsonLoose,
        Token::Ident(_, Keyword::JSON_MERGE) => Function::JsonMerge,
        Token::Ident(_, Keyword::JSON_MERGE_PATCH) => Function::JsonMergePath,
        Token::Ident(_, Keyword::JSON_MERGE_PRESERVE) => Function::JsonMergePerserve,
        Token::Ident(_, Keyword::JSON_NORMALIZE) => Function::JsonNormalize,
        Token::Ident(_, Keyword::JSON_OBJECT) => Function::JsonObject,
        Token::Ident(_, Keyword::JSON_OBJECT_FILTER_KEYS) => Function::JsonObjectFilterKeys,
        Token::Ident(_, Keyword::JSON_OBJECT_TO_ARRAY) => Function::JsonObjectToArray,
        Token::Ident(_, Keyword::JSON_OBJECTAGG) => Function::JsonObjectAgg,
        Token::Ident(_, Keyword::JSON_OVERLAPS) => Function::JsonOverlaps,
        Token::Ident(_, Keyword::JSON_PRETTY) => Function::JsonPretty,
        Token::Ident(_, Keyword::JSON_QUERY) => Function::JsonQuery,
        Token::Ident(_, Keyword::JSON_QUOTE) => Function::JsonQuote,
        Token::Ident(_, Keyword::JSON_REMOVE) => Function::JsonRemove,
        Token::Ident(_, Keyword::JSON_REPLACE) => Function::JsonReplace,
        Token::Ident(_, Keyword::JSON_SCHEMA_VALID) => Function::JsonSchemaValid,
        Token::Ident(_, Keyword::JSON_SEARCH) => Function::JsonSearch,
        Token::Ident(_, Keyword::JSON_SET) => Function::JsonSet,
        Token::Ident(_, Keyword::JSON_TABLE) => Function::JsonTable,
        Token::Ident(_, Keyword::JSON_TYPE) => Function::JsonType,
        Token::Ident(_, Keyword::JSON_UNQUOTE) => Function::JsonUnquote,
        Token::Ident(_, Keyword::JSON_VALID) => Function::JsonValid,
        Token::Ident(_, Keyword::JSON_VALUE) => Function::JsonValue,

        // Sqlite
        Token::Ident(_, Keyword::STRFTIME) => Function::Strftime,
        Token::Ident(_, Keyword::DATETIME) => Function::Datetime,
        Token::Ident(v, k) if !k.reserved() => Function::Other(v),
        _ => {
            parser.err("Unknown function", &span);
            Function::Unknown
        }
    };

    let mut args = Vec::new();
    if !matches!(parser.token, Token::RParen) {
        loop {
            parser.recovered(
                "')' or ','",
                &|t| matches!(t, Token::RParen | Token::Comma),
                |parser| {
                    args.push(parse_expression_outer(parser)?);
                    Ok(())
                },
            )?;
            if parser.skip_token(Token::Comma).is_none() {
                break;
            }
        }
    }
    parser.consume_token(Token::RParen)?;

    if let Some(over_span) = parser.skip_keyword(Keyword::OVER) {
        parser.consume_token(Token::LParen)?;
        let order_span = parser.consume_keywords(&[Keyword::ORDER, Keyword::BY])?;
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
        parser.consume_token(Token::RParen)?;
        Ok(Expression::WindowFunction {
            function: func,
            args,
            function_span: span,
            over_span,
            window_spec: WindowSpec {
                order_by: (order_span, order),
            },
        })
    } else {
        Ok(Expression::Function(func, args, span))
    }
}

//const INTERVAL_PRIORITY: usize = 10;
const IN_PRIORITY: usize = 110;

trait Priority {
    fn priority(&self) -> usize;
}

impl Priority for BinaryOperator {
    fn priority(&self) -> usize {
        match self {
            BinaryOperator::Or => 140,
            BinaryOperator::Xor => 150,
            BinaryOperator::And => 160,
            BinaryOperator::Eq => 110,
            BinaryOperator::NullSafeEq => 110,
            BinaryOperator::GtEq => 110,
            BinaryOperator::Gt => 110,
            BinaryOperator::LtEq => 110,
            BinaryOperator::Lt => 110,
            BinaryOperator::Neq => 110,
            BinaryOperator::Like => 110,
            BinaryOperator::NotLike => 110,
            BinaryOperator::ShiftLeft => 80,
            BinaryOperator::ShiftRight => 80,
            BinaryOperator::BitAnd => 90,
            BinaryOperator::BitOr => 100,
            BinaryOperator::BitXor => 50,
            BinaryOperator::Add => 70,
            BinaryOperator::Subtract => 70,
            BinaryOperator::Divide => 60,
            BinaryOperator::Div => 60,
            BinaryOperator::Mod => 60,
            BinaryOperator::Mult => 60,
        }
    }
}

impl Priority for UnaryOperator {
    fn priority(&self) -> usize {
        match self {
            UnaryOperator::Binary => 20,
            UnaryOperator::Collate => 20,
            UnaryOperator::LogicalNot => 30,
            UnaryOperator::Minus => 40,
            UnaryOperator::Not => 130,
        }
    }
}

#[derive(Debug)]
enum ReduceMember<'a> {
    Expression(Expression<'a>),
    Binary(BinaryOperator, Span),
    Unary(UnaryOperator, Span),
}

struct Reducer<'a> {
    stack: Vec<ReduceMember<'a>>,
}

impl<'a> Reducer<'a> {
    fn reduce(&mut self, priority: usize) -> Result<(), &'static str> {
        let mut e = match self.stack.pop() {
            Some(ReduceMember::Expression(e)) => e,
            _ => {
                return Err("Expected expression before here");
            }
        };
        loop {
            let v = self.stack.pop();
            match v {
                None => break,
                Some(ReduceMember::Expression(_)) => return Err("ICE Reduce stack error 1"),
                Some(ReduceMember::Unary(op, span)) if op.priority() > priority => {
                    self.stack.push(ReduceMember::Unary(op, span));
                    break;
                }
                Some(ReduceMember::Binary(op, span)) if op.priority() > priority => {
                    self.stack.push(ReduceMember::Binary(op, span));
                    break;
                }
                Some(ReduceMember::Unary(op, op_span)) => {
                    e = Expression::Unary {
                        op,
                        op_span,
                        operand: Box::new(e),
                    };
                }
                Some(ReduceMember::Binary(op, op_span)) => {
                    let lhs = match self.stack.pop() {
                        Some(ReduceMember::Expression(e)) => e,
                        _ => return Err("ICE Reduce stack error 2"),
                    };
                    e = Expression::Binary {
                        op,
                        op_span,
                        lhs: Box::new(lhs),
                        rhs: Box::new(e),
                    };
                }
            }
        }
        self.stack.push(ReduceMember::Expression(e));
        Ok(())
    }

    fn shift_binop(&mut self, span: Span, op: BinaryOperator) -> Result<(), &'static str> {
        self.reduce(op.priority())?;
        self.stack.push(ReduceMember::Binary(op, span));
        Ok(())
    }

    fn shift_unary(&mut self, span: Span, op: UnaryOperator) -> Result<(), &'static str> {
        if matches!(self.stack.last(), Some(ReduceMember::Expression(_))) {
            return Err("Unary operator cannot come before expression");
        }
        self.stack.push(ReduceMember::Unary(op, span));
        Ok(())
    }

    fn shift_expr(&mut self, e: Expression<'a>) -> Result<(), &'static str> {
        if matches!(self.stack.last(), Some(ReduceMember::Expression(_))) {
            //panic!();
            return Err("Expression should not follow expression");
        }
        self.stack.push(ReduceMember::Expression(e));
        Ok(())
    }
}

pub(crate) fn parse_expression<'a>(
    parser: &mut Parser<'a, '_>,
    inner: bool,
) -> Result<Expression<'a>, ParseError> {
    let mut r = Reducer { stack: Vec::new() };
    loop {
        let e = match &parser.token {
            Token::Ident(_, Keyword::OR) | Token::DoublePipe if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::Or)
            }
            Token::Ident(_, Keyword::XOR) if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::Xor)
            }
            Token::Ident(_, Keyword::AND) | Token::DoubleAmpersand if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::And)
            }
            Token::Eq if !inner => r.shift_binop(parser.consume(), BinaryOperator::Eq),
            Token::Spaceship if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::NullSafeEq)
            }
            Token::GtEq if !inner => r.shift_binop(parser.consume(), BinaryOperator::GtEq),
            Token::Gt if !inner => r.shift_binop(parser.consume(), BinaryOperator::Gt),
            Token::LtEq if !inner => r.shift_binop(parser.consume(), BinaryOperator::LtEq),
            Token::Lt if !inner => r.shift_binop(parser.consume(), BinaryOperator::Lt),
            Token::Neq if !inner => r.shift_binop(parser.consume(), BinaryOperator::Neq),
            Token::ShiftLeft if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::ShiftLeft)
            }
            Token::ShiftRight if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::ShiftRight)
            }
            Token::Ampersand => r.shift_binop(parser.consume(), BinaryOperator::BitAnd),
            Token::Pipe if !inner => r.shift_binop(parser.consume(), BinaryOperator::BitOr),
            Token::Ident(_, Keyword::BINARY) if !inner => {
                r.shift_unary(parser.consume(), UnaryOperator::Binary)
            }
            Token::Ident(_, Keyword::COLLATE) if !inner => {
                r.shift_unary(parser.consume(), UnaryOperator::Collate)
            }
            Token::ExclamationMark if !inner => {
                r.shift_unary(parser.consume(), UnaryOperator::LogicalNot)
            }
            Token::Minus if !matches!(r.stack.last(), Some(ReduceMember::Expression(_))) => {
                r.shift_unary(parser.consume(), UnaryOperator::Minus)
            }
            Token::Minus
                if !inner && matches!(r.stack.last(), Some(ReduceMember::Expression(_))) =>
            {
                r.shift_binop(parser.consume(), BinaryOperator::Subtract)
            }
            Token::Ident(_, Keyword::IN) if !inner => {
                if let Err(e) = r.reduce(IN_PRIORITY) {
                    parser.err_here(e)?;
                }
                let lhs = match r.stack.pop() {
                    Some(ReduceMember::Expression(e)) => e,
                    _ => parser.err_here("Expected expression before here 3")?,
                };
                let op = parser.consume_keyword(Keyword::IN)?;
                parser.consume_token(Token::LParen)?;
                let mut rhs = Vec::new();
                loop {
                    parser.recovered(
                        "')' or ','",
                        &|t| matches!(t, Token::RParen | Token::Comma),
                        |parser| {
                            rhs.push(parse_expression_paren(parser)?);
                            Ok(())
                        },
                    )?;
                    if parser.skip_token(Token::Comma).is_none() {
                        break;
                    }
                }
                parser.consume_token(Token::RParen)?;
                r.shift_expr(Expression::In {
                    lhs: Box::new(lhs),
                    rhs,
                    in_span: op,
                    not_in: false,
                })
            }
            Token::Ident(_, Keyword::IS) if !inner => {
                if let Err(e) = r.reduce(IN_PRIORITY) {
                    parser.err_here(e)?;
                }
                let lhs = match r.stack.pop() {
                    Some(ReduceMember::Expression(e)) => e,
                    _ => parser.err_here("Expected expression before here 4")?,
                };
                let op = parser.consume_keyword(Keyword::IS)?;
                let (is, op) = match &parser.token {
                    Token::Ident(_, Keyword::NOT) => {
                        parser.consume();
                        match &parser.token {
                            Token::Ident(_, Keyword::TRUE) => {
                                (Is::NotTrue, parser.consume().join_span(&op))
                            }
                            Token::Ident(_, Keyword::FALSE) => {
                                (Is::NotFalse, parser.consume().join_span(&op))
                            }
                            Token::Ident(_, Keyword::NULL) => {
                                (Is::NotNull, parser.consume().join_span(&op))
                            }
                            Token::Ident(_, Keyword::UNKNOWN) => {
                                (Is::NotUnknown, parser.consume().join_span(&op))
                            }
                            _ => parser.expected_failure("'TRUE', 'FALSE', 'UNKNOWN' or 'NULL'")?,
                        }
                    }
                    Token::Ident(_, Keyword::TRUE) => (Is::True, parser.consume().join_span(&op)),
                    Token::Ident(_, Keyword::FALSE) => (Is::False, parser.consume().join_span(&op)),
                    Token::Ident(_, Keyword::NULL) => (Is::Null, parser.consume().join_span(&op)),
                    Token::Ident(_, Keyword::UNKNOWN) => {
                        (Is::Unknown, parser.consume().join_span(&op))
                    }
                    _ => parser.expected_failure("'NOT', 'TRUE', 'FALSE', 'UNKNOWN' or 'NULL'")?,
                };
                r.shift_expr(Expression::Is(Box::new(lhs), is, op))
            }
            Token::Ident(_, Keyword::NOT)
                if !matches!(r.stack.last(), Some(ReduceMember::Expression(_))) =>
            {
                r.shift_unary(parser.consume(), UnaryOperator::Not)
            }
            Token::Ident(_, Keyword::NOT)
                if !inner && matches!(r.stack.last(), Some(ReduceMember::Expression(_))) =>
            {
                if let Err(e) = r.reduce(IN_PRIORITY) {
                    parser.err_here(e)?;
                }
                let lhs = match r.stack.pop() {
                    Some(ReduceMember::Expression(e)) => e,
                    _ => parser.err_here("Expected expression before here 2")?,
                };
                let op = parser.consume_keyword(Keyword::NOT)?;
                match &parser.token {
                    Token::Ident(_, Keyword::IN) => {
                        let op = parser.consume_keyword(Keyword::IN)?.join_span(&op);
                        parser.consume_token(Token::LParen)?;
                        let mut rhs = Vec::new();
                        loop {
                            parser.recovered(
                                "')' or ','",
                                &|t| matches!(t, Token::RParen | Token::Comma),
                                |parser| {
                                    rhs.push(parse_expression_paren(parser)?);
                                    Ok(())
                                },
                            )?;
                            if parser.skip_token(Token::Comma).is_none() {
                                break;
                            }
                        }
                        parser.consume_token(Token::RParen)?;
                        r.shift_expr(Expression::In {
                            lhs: Box::new(lhs),
                            rhs,
                            in_span: op,
                            not_in: true,
                        })
                    }
                    Token::Ident(_, Keyword::LIKE) => {
                        r.stack.push(ReduceMember::Expression(lhs));
                        r.shift_binop(parser.consume().join_span(&op), BinaryOperator::NotLike)
                    }
                    _ => parser.expected_failure("'IN' or 'LIKE'")?,
                }
            }
            Token::Ident(_, Keyword::LIKE) if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::Like)
            }
            Token::Ident(_, Keyword::INTERVAL) => {
                let interval_span = parser.consume();
                let time_interval = match parser.token {
                    Token::SingleQuotedString(_) | Token::DoubleQuotedString(_) => {
                        let v = parser.consume_string()?;
                        let mut r = Vec::new();
                        for part in v.split([':', '!', ',', '.', '-', ' ']) {
                            let Ok(v) = part.parse() else {
                                parser.err("Expected . separated integers in a string", &v);
                                continue;
                            };
                            r.push(v);
                        }
                        (r, v.span())
                    }
                    Token::Integer(_) => {
                        let (v, s) = parser.consume_int()?;
                        (vec![v], s)
                    }
                    _ => parser.err_here("Expected integer or string")?,
                };
                let Some(u) = parse_time_unit(&parser.token) else {
                    parser.err_here("Expected time unit")?
                };
                let time_unit = (u, parser.consume());
                let e = Expression::Interval {
                    interval_span,
                    time_interval,
                    time_unit,
                };
                r.shift_expr(e)
            }
            Token::Ident(_, Keyword::TIMESTAMPADD) => {
                let timestamp_add_span = parser.consume();
                parser.consume_token(Token::LParen)?;
                let parts = parser.recovered("')'", &|t| matches!(t, Token::RParen), |parser| {
                    let Some(u) = parse_time_unit(&parser.token) else {
                        parser.err_here("Expected time unit")?
                    };
                    let unit = (u, parser.consume());
                    parser.consume_token(Token::Comma)?;
                    let interval = parse_expression_outer(parser)?;
                    parser.consume_token(Token::Comma)?;
                    let datetime = parse_expression_outer(parser)?;
                    Ok(Some((unit, Box::new(interval), Box::new(datetime))))
                })?;
                parser.consume_token(Token::RParen)?;
                if let Some((unit, interval, datetime)) = parts {
                    r.shift_expr(Expression::TimestampAdd {
                        timestamp_add_span,
                        unit,
                        interval,
                        datetime,
                    })
                } else {
                    r.shift_expr(Expression::Invalid(timestamp_add_span))
                }
            }
            Token::Ident(_, Keyword::TIMESTAMPDIFF) => {
                let timestamp_diff_span = parser.consume();
                parser.consume_token(Token::LParen)?;
                let parts = parser.recovered("')'", &|t| matches!(t, Token::RParen), |parser| {
                    let Some(u) = parse_time_unit(&parser.token) else {
                        parser.err_here("Expected time unit")?
                    };
                    let unit = (u, parser.consume());
                    parser.consume_token(Token::Comma)?;
                    let e1 = parse_expression_outer(parser)?;
                    parser.consume_token(Token::Comma)?;
                    let e2 = parse_expression_outer(parser)?;
                    Ok(Some((unit, Box::new(e1), Box::new(e2))))
                })?;
                parser.consume_token(Token::RParen)?;
                if let Some((unit, e1, e2)) = parts {
                    r.shift_expr(Expression::TimestampDiff {
                        timestamp_diff_span,
                        unit,
                        e1,
                        e2,
                    })
                } else {
                    r.shift_expr(Expression::Invalid(timestamp_diff_span))
                }
            }
            Token::Plus if !inner => r.shift_binop(parser.consume(), BinaryOperator::Add),
            Token::Div if !inner => r.shift_binop(parser.consume(), BinaryOperator::Divide),
            Token::Minus if !inner => r.shift_binop(parser.consume(), BinaryOperator::Subtract),
            Token::Ident(_, Keyword::LIKE) if !inner => {
                r.shift_binop(parser.consume(), BinaryOperator::Like)
            }
            Token::Mul if !matches!(r.stack.last(), Some(ReduceMember::Expression(_))) => r
                .shift_expr(Expression::Identifier(vec![IdentifierPart::Star(
                    parser.consume_token(Token::Mul)?,
                )])),
            Token::Mul if !inner && matches!(r.stack.last(), Some(ReduceMember::Expression(_))) => {
                r.shift_binop(parser.consume(), BinaryOperator::Mult)
            }
            Token::Ident(_, Keyword::TRUE) => r.shift_expr(Expression::Bool(
                true,
                parser.consume_keyword(Keyword::TRUE)?,
            )),
            Token::Ident(_, Keyword::FALSE) => r.shift_expr(Expression::Bool(
                false,
                parser.consume_keyword(Keyword::FALSE)?,
            )),
            Token::Ident(_, Keyword::NULL) => {
                r.shift_expr(Expression::Null(parser.consume_keyword(Keyword::NULL)?))
            }
            Token::Ident(_, Keyword::_LIST_) if parser.options.list_hack => {
                let arg = parser.arg;
                parser.arg += 1;
                r.shift_expr(Expression::ListHack((
                    arg,
                    parser.consume_keyword(Keyword::_LIST_)?,
                )))
            }
            Token::SingleQuotedString(_) | Token::DoubleQuotedString(_) => {
                r.shift_expr(Expression::String(parser.consume_string()?))
            }
            Token::Integer(_) => r.shift_expr(Expression::Integer(parser.consume_int()?)),
            Token::Float(_) => r.shift_expr(Expression::Float(parser.consume_float()?)),

            Token::Ident(_, Keyword::CAST) => {
                let cast_span = parser.consume_keyword(Keyword::CAST)?;
                parser.consume_token(Token::LParen)?;
                let cast = parser.recovered("')'", &|t| matches!(t, Token::RParen), |parser| {
                    let expr = parse_expression_outer(parser)?;
                    let as_span = parser.consume_keyword(Keyword::AS)?;
                    let type_ = parse_data_type(parser, false)?;
                    Ok(Some((expr, as_span, type_)))
                })?;
                parser.consume_token(Token::RParen)?;
                if let Some((expr, as_span, type_)) = cast {
                    r.shift_expr(Expression::Cast {
                        cast_span,
                        expr: Box::new(expr),
                        as_span,
                        type_,
                    })
                } else {
                    r.shift_expr(Expression::Invalid(cast_span))
                }
            }
            Token::Ident(_, Keyword::COUNT) => {
                let count_span = parser.consume_keyword(Keyword::COUNT)?;
                parser.consume_token(Token::LParen)?;
                let distinct_span = parser.skip_keyword(Keyword::DISTINCT);
                let expr = parser.recovered("')'", &|t| matches!(t, Token::RParen), |parser| {
                    let expr = parse_expression_outer(parser)?;
                    Ok(Some(expr))
                })?;
                parser.consume_token(Token::RParen)?;
                if let Some(expr) = expr {
                    r.shift_expr(Expression::Count {
                        count_span,
                        distinct_span,
                        expr: Box::new(expr),
                    })
                } else {
                    r.shift_expr(Expression::Invalid(count_span))
                }
            }
            Token::Ident(_, Keyword::GROUP_CONCAT) => {
                let group_concat_span: core::ops::Range<usize> =
                    parser.consume_keyword(Keyword::GROUP_CONCAT)?;
                parser.consume_token(Token::LParen)?;
                let distinct_span: Option<core::ops::Range<usize>> =
                    parser.skip_keyword(Keyword::DISTINCT);
                let expr = parser.recovered("')'", &|t| matches!(t, Token::RParen), |parser| {
                    let expr = parse_expression_outer(parser)?;
                    Ok(Some(expr))
                })?;
                // TODO
                // [ORDER BY {unsigned_integer | col_name | expr}
                //     [ASC | DESC] [,col_name ...]]
                // [SEPARATOR str_val]
                // [LIMIT {[offset,] row_count | row_count OFFSET offset}])
                parser.consume_token(Token::RParen)?;
                if let Some(expr) = expr {
                    r.shift_expr(Expression::GroupConcat {
                        group_concat_span,
                        distinct_span,
                        expr: Box::new(expr),
                    })
                } else {
                    r.shift_expr(Expression::Invalid(group_concat_span))
                }
            }
            Token::Ident(_, Keyword::EXTRACT) => {
                let extract_span = parser.consume_keyword(Keyword::EXTRACT)?;
                parser.consume_token(Token::LParen)?;
                let parts = parser.recovered("')'", &|t| matches!(t, Token::RParen), |parser| {
                    let Some(u) = parse_time_unit(&parser.token) else {
                        parser.err_here("Expected time unit")?
                    };
                    let time_unit = (u, parser.consume());
                    let from_span = parser.consume_keyword(Keyword::FROM)?;
                    let date = parse_expression_outer(parser)?;
                    Ok(Some((time_unit, from_span, Box::new(date))))
                })?;
                parser.consume_token(Token::RParen)?;
                if let Some((time_unit, from_span, date)) = parts {
                    r.shift_expr(Expression::Extract {
                        extract_span,
                        time_unit,
                        from_span,
                        date,
                    })
                } else {
                    r.shift_expr(Expression::Invalid(extract_span))
                }
            }
            Token::Ident(_, k) if k.expr_ident() => {
                let i = parser.token.clone();
                let s = parser.span.clone();
                parser.consume();
                if matches!(parser.token, Token::LParen) {
                    r.shift_expr(parse_function(parser, i, s)?)
                } else {
                    let f = match i {
                        Token::Ident(_, Keyword::CURRENT_TIMESTAMP) => {
                            Some(Function::CurrentTimestamp)
                        }
                        Token::Ident(_, Keyword::LOCALTIME | Keyword::LOCALTIMESTAMP) => {
                            Some(Function::Now)
                        }
                        Token::Ident(_, Keyword::UTC_TIMESTAMP) => Some(Function::UtcTimeStamp),
                        Token::Ident(_, Keyword::UTC_DATE) => Some(Function::UtcDate),
                        Token::Ident(_, Keyword::UTC_TIME) => Some(Function::UtcTime),
                        Token::Ident(_, Keyword::CURRENT_DATE) => Some(Function::CurDate),
                        Token::Ident(_, Keyword::CURRENT_TIME) => Some(Function::CurTime),
                        _ => None,
                    };
                    if let Some(f) = f {
                        r.shift_expr(Expression::Function(f, Vec::new(), s))
                    } else {
                        let mut parts = vec![IdentifierPart::Name(
                            parser.token_to_plain_identifier(&i, s)?,
                        )];
                        loop {
                            if parser.skip_token(Token::Period).is_none() {
                                break;
                            }
                            match &parser.token {
                                Token::Mul => parts
                                    .push(IdentifierPart::Star(parser.consume_token(Token::Mul)?)),
                                Token::Ident(_, _) => parts
                                    .push(IdentifierPart::Name(parser.consume_plain_identifier()?)),
                                _ => parser.expected_failure("Identifier or '*'")?,
                            }
                        }
                        r.shift_expr(Expression::Identifier(parts))
                    }
                }
            }
            Token::QuestionMark
                if matches!(parser.options.arguments, crate::SQLArguments::QuestionMark) =>
            {
                let arg = parser.arg;
                parser.arg += 1;
                r.shift_expr(Expression::Arg((
                    arg,
                    parser.consume_token(Token::QuestionMark)?,
                )))
            }
            Token::PercentS if matches!(parser.options.arguments, crate::SQLArguments::Percent) => {
                let arg = parser.arg;
                parser.arg += 1;
                r.shift_expr(Expression::Arg((
                    arg,
                    parser.consume_token(Token::PercentS)?,
                )))
            }
            Token::DollarArg(arg)
                if matches!(parser.options.arguments, crate::SQLArguments::Dollar) =>
            {
                r.shift_expr(Expression::Arg((arg - 1, parser.consume())))
            }
            Token::LParen => {
                parser.consume_token(Token::LParen)?;
                let ans = parse_expression_paren(parser)?;
                parser.consume_token(Token::RParen)?;
                r.shift_expr(ans)
            }
            Token::Ident(_, Keyword::EXISTS) => {
                parser.consume_keyword(Keyword::EXISTS)?;
                parser.consume_token(Token::LParen)?;
                let ans = Expression::Exists(Box::new(parse_compound_query(parser)?));
                parser.consume_token(Token::RParen)?;
                r.shift_expr(ans)
            }
            Token::Ident(_, Keyword::CASE) => {
                let case_span = parser.consume_keyword(Keyword::CASE)?;
                let value = if !matches!(parser.token, Token::Ident(_, Keyword::WHEN)) {
                    Some(Box::new(parse_expression(parser, false)?))
                } else {
                    None
                };
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
                            let then = parse_expression(parser, false)?;
                            whens.push(When {
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
                            else_ = Some((span, Box::new(parse_expression(parser, false)?)))
                        };
                        Ok(())
                    },
                )?;
                let end_span = parser.consume_keyword(Keyword::END)?;
                r.shift_expr(Expression::Case {
                    case_span,
                    value,
                    whens,
                    else_,
                    end_span,
                })
            }
            Token::AtAtGlobal | Token::AtAtSession => {
                let global = parser.skip_token(Token::AtAtGlobal);
                let session = if global.is_none() {
                    Some(parser.consume_token(Token::AtAtSession)?)
                } else {
                    None
                };
                let dot = Some(parser.consume_token(Token::Period)?);
                let variable = match &parser.token {
                    Token::Ident(_, Keyword::TIME_ZONE) => Variable::TimeZone,
                    Token::Ident(t, _) => Variable::Other(t),
                    _ => parser.expected_failure("Identifier")?,
                };
                let variable_span = parser.consume();
                r.shift_expr(Expression::Variable {
                    global,
                    session,
                    dot,
                    variable,
                    variable_span,
                })
            }
            _ => break,
        };
        if let Err(e) = e {
            parser.err_here(e.to_string())?;
        }
    }
    if r.reduce(99999).is_err() {
        parser.err_here("Expected expression")
    } else if r.stack.len() != 1 {
        parser.ice(file!(), line!())
    } else if let Some(ReduceMember::Expression(e)) = r.stack.pop() {
        Ok(e)
    } else {
        parser.ice(file!(), line!())
    }
}

pub(crate) fn parse_expression_outer<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<Expression<'a>, ParseError> {
    if matches!(parser.token, Token::Ident(_, Keyword::SELECT)) {
        Ok(Expression::Subquery(Box::new(Statement::Select(
            parse_select(parser)?,
        ))))
    } else {
        parse_expression(parser, false)
    }
}

pub(crate) fn parse_expression_paren<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<Expression<'a>, ParseError> {
    if matches!(parser.token, Token::Ident(_, Keyword::SELECT)) {
        Ok(Expression::Subquery(Box::new(parse_compound_query(
            parser,
        )?)))
    } else {
        parse_expression(parser, false)
    }
}

#[cfg(test)]
mod tests {
    use core::ops::Deref;

    use alloc::{
        format,
        string::{String, ToString},
    };

    use crate::{
        expression::{BinaryOperator, Expression},
        issue::Issues,
        parser::Parser,
        Function, ParseOptions, SQLDialect,
    };

    use super::{parse_expression, IdentifierPart};

    fn test_ident<'a>(e: impl AsRef<Expression<'a>>, v: &str) -> Result<(), String> {
        let v = match e.as_ref() {
            Expression::Identifier(a) => match a.as_slice() {
                [IdentifierPart::Name(vv)] => vv.deref() == v,
                _ => false,
            },
            _ => false,
        };
        if !v {
            Err(format!("Expected identifier {} found {:?}", v, e.as_ref()))
        } else {
            Ok(())
        }
    }

    fn test_expr(src: &'static str, f: impl FnOnce(&Expression<'_>) -> Result<(), String>) {
        let mut issues = Issues::new(src);
        let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
        let mut parser = Parser::new(src, &mut issues, &options);
        let res = parse_expression(&mut parser, false).expect("Expression in test expr");
        if let Err(e) = f(&res) {
            panic!("Error parsing {}: {}\nGot {:#?}", src, e, res);
        }
    }

    #[test]
    fn expressions() {
        test_expr("`a` + `b` * `c` + `d`", |e| {
            match e {
                Expression::Binary {
                    op: BinaryOperator::Add,
                    lhs,
                    rhs,
                    ..
                } => {
                    match lhs.as_ref() {
                        Expression::Binary {
                            op: BinaryOperator::Add,
                            lhs,
                            rhs,
                            ..
                        } => {
                            test_ident(lhs, "a")?;
                            match rhs.as_ref() {
                                Expression::Binary {
                                    op: BinaryOperator::Mult,
                                    lhs,
                                    rhs,
                                    ..
                                } => {
                                    test_ident(lhs, "b")?;
                                    test_ident(rhs, "c")?;
                                }
                                _ => return Err("Lhs.Rhs".to_string()),
                            }
                        }
                        _ => return Err("Lhs".to_string()),
                    }
                    test_ident(rhs, "d")?;
                }
                _ => return Err("Outer".to_string()),
            }
            Ok(())
        });
    }

    #[test]
    fn mariadb_datetime_functions() {
        fn test_func(src: &'static str, f: Function, cnt: usize) {
            let mut issues = Issues::new(src);
            let options = ParseOptions::new().dialect(SQLDialect::MariaDB);
            let mut parser = Parser::new(src, &mut issues, &options);
            let res = match parse_expression(&mut parser, false) {
                Ok(res) => res,
                Err(e) => panic!("Unable to parse {}: {:?}", src, e),
            };
            let Expression::Function(pf, args, _) = res else {
                panic!("Should be parsed as function {}", src);
            };
            assert_eq!(pf, f, "Failure en expr {}", src);
            assert_eq!(args.len(), cnt, "Failure en expr {}", src);
        }
        test_func("ADD_MONTHS('2012-01-31', 2)", Function::AddMonths, 2);
        test_func(
            "ADDTIME('2007-12-31 23:59:59.999999', '1 1:1:1.000002')",
            Function::AddTime,
            2,
        );
        test_func(
            "DATE_ADD('2008-01-02', INTERVAL 31 DAY)",
            Function::AddDate,
            2,
        );
        test_func(
            "ADDDATE('2008-01-02', INTERVAL 31 DAY)",
            Function::AddDate,
            2,
        );
        test_func("ADDDATE('2008-01-02', 31)", Function::AddDate, 2);
        test_func(
            "CONVERT_TZ('2016-01-01 12:00:00','+00:00','+10:00')",
            Function::ConvertTz,
            3,
        );
        test_func("CURDATE()", Function::CurDate, 0);
        test_func("CURRENT_DATE", Function::CurDate, 0);
        test_func("CURRENT_DATE()", Function::CurDate, 0);
        test_func("CURRENT_TIME", Function::CurTime, 0);
        test_func("CURRENT_TIME()", Function::CurTime, 0);
        test_func("CURTIME()", Function::CurTime, 0);
        test_func("CURTIME(2)", Function::CurTime, 1);
        test_func("CURRENT_DATE", Function::CurDate, 0);
        test_func("CURRENT_DATE()", Function::CurDate, 0);
        test_func("CURDATE()", Function::CurDate, 0);
        test_func("CURRENT_TIMESTAMP", Function::CurrentTimestamp, 0);
        test_func("CURRENT_TIMESTAMP()", Function::CurrentTimestamp, 0);
        test_func("CURRENT_TIMESTAMP(10)", Function::CurrentTimestamp, 1);
        test_func("LOCALTIME", Function::Now, 0);
        test_func("LOCALTIME()", Function::Now, 0);
        test_func("LOCALTIME(10)", Function::Now, 1);
        test_func("LOCALTIMESTAMP", Function::Now, 0);
        test_func("LOCALTIMESTAMP()", Function::Now, 0);
        test_func("LOCALTIMESTAMP(10)", Function::Now, 1);
        test_func("DATE('2013-07-18 12:21:32')", Function::Date, 1);
        test_func(
            "DATE_FORMAT('2009-10-04 22:23:00', '%W %M %Y')",
            Function::DateFormat,
            2,
        );
        test_func(
            "DATE_SUB('1998-01-02', INTERVAL 31 DAY)",
            Function::DateSub,
            2,
        );
        test_func("DAY('2007-02-03')", Function::DayOfMonth, 1);
        test_func("DAYOFMONTH('2007-02-03')", Function::DayOfMonth, 1);
        test_func(
            "DATEDIFF('2007-12-31 23:59:59','2007-12-30')",
            Function::DateDiff,
            2,
        );
        test_func("DAYNAME('2007-02-03')", Function::DayName, 1);
        test_func("DAYOFYEAR('2018-02-16')", Function::DayOfYear, 1);
        test_func("DAYOFWEEK('2007-02-03')", Function::DayOfWeek, 1);
        test_expr("EXTRACT(YEAR_MONTH FROM '2009-07-02 01:02:03')", |e| {
            let Expression::Extract { .. } = e else {
                return Err("Wrong type".to_string());
            };
            Ok(())
        });
        //test_func("FORMAT_PICO_TIME(4321123443212345) AS h", Function::DayOfWeek, 1);
        test_func("FROM_DAYS(730669)", Function::FromDays, 1);
        test_func("FROM_UNIXTIME(1196440219)", Function::FromUnixTime, 1);
        test_func(
            "FROM_UNIXTIME(UNIX_TIMESTAMP(), '%Y %D %M %h:%i:%s %x')",
            Function::FromUnixTime,
            2,
        );
        //test_func("GET_FORMAT(DATE, 'EUR')", Function::GetFormat, 2);
        test_func("HOUR('10:05:03')", Function::Hour, 1);
        test_func("LAST_DAY('2004-01-01 01:01:01')", Function::LastDay, 1);
        test_func("MAKEDATE(2011,31)", Function::MakeDate, 2);
        test_func("MAKETIME(-13,57,33)", Function::MakeTime, 3);
        test_func("MICROSECOND('12:00:00.123456')", Function::MicroSecond, 1);
        test_func("MINUTE('2013-08-03 11:04:03')", Function::Minute, 1);
        test_func("MONTH('2019-01-03')", Function::Month, 1);
        test_func("MONTHNAME('2019-02-03')", Function::MonthName, 1);
        test_func("PERIOD_ADD(200801,2)", Function::PeriodAdd, 2);
        test_func("PERIOD_DIFF(200802,200703)", Function::PeriodDiff, 2);
        test_func("QUARTER('2008-04-01')", Function::Quarter, 1);
        test_func("SEC_TO_TIME(12414)", Function::SecToTime, 1);
        test_func("SECOND('10:05:03')", Function::Second, 1);
        test_func(
            "STR_TO_DATE('Wednesday, June 2, 2014', '%W, %M %e, %Y')",
            Function::StrToDate,
            2,
        );
        test_func(
            "DATE_SUB('2008-01-02', INTERVAL 31 DAY)",
            Function::DateSub,
            2,
        );
        test_func("SUBDATE('2008-01-02 12:00:00', 31)", Function::DateSub, 2);
        test_func(
            "SUBDATE('2008-01-02', INTERVAL 31 DAY)",
            Function::DateSub,
            2,
        );
        test_func(
            "SUBTIME('2007-12-31 23:59:59.999999','1 1:1:1.000002')",
            Function::SubTime,
            2,
        );
        test_func("SYSDATE()", Function::SysDate, 0);
        test_func("SYSDATE(4)", Function::SysDate, 1);
        test_func("TIME('2003-12-31 01:02:03')", Function::Time, 1);
        test_func(
            "TIME_FORMAT('100:00:00', '%H %k %h %I %l')",
            Function::TimeFormat,
            2,
        );
        test_func("TIME_TO_SEC('22:23:00')", Function::TimeToSec, 1);
        test_func(
            "TIMEDIFF('2008-12-31 23:59:59.000001', '2008-12-30 01:01:01.000002')",
            Function::TimeDiff,
            2,
        );
        test_func("TIMESTAMP('2003-12-31')", Function::Timestamp, 1);
        test_func(
            "TIMESTAMP('2003-12-31 12:00:00','6:30:00')",
            Function::Timestamp,
            2,
        );
        test_expr("TIMESTAMPADD(MINUTE,1,'2003-01-02')", |e| {
            let Expression::TimestampAdd { .. } = e else {
                return Err("Wrong type".to_string());
            };
            Ok(())
        });
        test_expr("TIMESTAMPDIFF(MONTH,'2003-02-01','2003-05-01');", |e| {
            let Expression::TimestampDiff { .. } = e else {
                return Err("Wrong type".to_string());
            };
            Ok(())
        });
        test_func("TO_DAYS('2007-10-07')", Function::ToDays, 1);
        test_func("UNIX_TIMESTAMP()", Function::UnixTimestamp, 0);
        test_func(
            "UNIX_TIMESTAMP('2007-11-30 10:30:19')",
            Function::UnixTimestamp,
            1,
        );
        test_func("UTC_DATE", Function::UtcDate, 0);
        test_func("UTC_DATE()", Function::UtcDate, 0);
        test_func("UTC_TIME", Function::UtcTime, 0);
        test_func("UTC_TIME()", Function::UtcTime, 0);
        test_func("UTC_TIME(5)", Function::UtcTime, 1);
        test_func("UTC_TIMESTAMP", Function::UtcTimeStamp, 0);
        test_func("UTC_TIMESTAMP()", Function::UtcTimeStamp, 0);
        test_func("UTC_TIMESTAMP(4)", Function::UtcTimeStamp, 1);
        test_func("WEEK('2008-02-20')", Function::Week, 1);
        test_func("WEEK('2008-02-20',0)", Function::Week, 2);
        test_func("WEEKDAY('2008-02-03 22:23:00')", Function::Weekday, 1);
        test_func("WEEKOFYEAR('2008-02-20')", Function::WeekOfYear, 1);
        test_func("YEAR('1987-01-01')", Function::Year, 1);
        test_func("YEARWEEK('1987-01-01')", Function::YearWeek, 1);
        test_func("YEARWEEK('1987-01-01',0)", Function::YearWeek, 2);
    }
}
