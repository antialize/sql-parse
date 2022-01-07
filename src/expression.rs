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

use std::borrow::Cow;

use crate::{
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser, SingleQuotedString},
    select::{parse_select, Select},
    Span, Spanned,
};

#[derive(Debug, Clone)]
pub enum Function {
    Ascii,
    Bin,
    BitLength,
    CharacterLength,
    Chr,
    Concat,
    ConcatWs,
    Elt,
    ExportSet,
    ExtractValue,
    Field,
    FindInSet,
    Format,
    FromBase64,
    Hex,
    Insert,
    InStr,
    LCase,
    Left,
    Length,
    LengthB,
    LoadFile,
    Locate,
    Lower,
    LPad,
    LTrim,
    MakeSet,
    Mid,
    NaturalSortkey,
    OctetLength,
    Ord,
    Position,
    Quote,
    Repeat,
    Replace,
    Reverse,
    Right,
    RPad,
    RTrim,
    SoundEx,
    Space,
    StrCmp,
    SubStr,
    SubStringIndex,
    ToBase64,
    ToChar,
    UCase,
    UncompressedLength,
    UnHex,
    UpdateXml,
    Upper,
    SFormat,
    CurrentTimestamp,
    Count,
    JsonExtract,
    IfNull,
    Exists,
}

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
}

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

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Binary,
    Collate,
    LogicalNot,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Binary {
        op: BinaryOperator,
        op_span: Span,
        lhs: Box<Expression<'a>>,
        rhs: Box<Expression<'a>>,
    },
    Unary {
        op: UnaryOperator,
        op_span: Span,
        operand: Box<Expression<'a>>,
    },
    Subquery(Box<Select<'a>>),
    Null(Span),
    Bool(bool, Span),
    String((Cow<'a, str>, Span)),
    Integer((u64, Span)),
    Float((f64, Span)),
    Function(Function, Vec<Expression<'a>>, Span),
    Identifier(Vec<(&'a str, Span)>),
    Arg((usize, Span)),
    Star(Span),
    In {
        lhs: Box<Expression<'a>>,
        rhs: Vec<Expression<'a>>,
        in_span: Span,
        not_in: bool,
    },
    Is(Box<Expression<'a>>, Is, Span),
}

pub(crate) fn parse_function<'a>(
    parser: &mut Parser<'a>,
    func: Function,
    allow_no_args: bool,
) -> Result<Expression<'a>, ParseError> {
    let span = parser.consume();

    if allow_no_args {
        if parser.skip_token(Token::LParen).is_none() {
            return Ok(Expression::Function(func, Vec::new(), span));
        }
    } else {
        parser.consume_token(Token::LParen)?;
    };
    let mut args = Vec::new();
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
    parser.consume_token(Token::RParen)?;

    Ok(Expression::Function(func, args, span))
}

const interval_priority: usize = 10;
const in_priority: usize = 110;

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
            _ => return Err("Expected expression before here"),
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
            return Err("Expression should not follow expression");
        }
        self.stack.push(ReduceMember::Expression(e));
        Ok(())
    }
}

pub(crate) fn parse_expression<'a>(
    parser: &mut Parser<'a>,
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
                if let Err(e) = r.reduce(in_priority) {
                    parser.error(e)?;
                }
                let lhs = match r.stack.pop() {
                    Some(ReduceMember::Expression(e)) => e,
                    _ => parser.error("Expected expression before here")?,
                };
                let op = parser.consume_keyword(Keyword::IN)?;
                parser.consume_token(Token::LParen)?;
                let mut rhs = Vec::new();
                loop {
                    parser.recovered(
                        "')' or ','",
                        &|t| matches!(t, Token::RParen | Token::Comma),
                        |parser| {
                            rhs.push(parse_expression_outer(parser)?);
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
                if let Err(e) = r.reduce(in_priority) {
                    parser.error(e)?;
                }
                let lhs = match r.stack.pop() {
                    Some(ReduceMember::Expression(e)) => e,
                    _ => parser.error("Expected expression before here")?,
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
                if let Err(e) = r.reduce(in_priority) {
                    parser.error(e)?;
                }
                let lhs = match r.stack.pop() {
                    Some(ReduceMember::Expression(e)) => e,
                    _ => parser.error("Expected expression before here")?,
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
                                    rhs.push(parse_expression_outer(parser)?);
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
                    _ => parser.expected_failure("'IN' or 'LIKE'")?,
                }
            }
            Token::Mul if !inner => {
                r.shift_expr(Expression::Star(parser.consume_token(Token::Mul)?))
            } //TODO
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

            Token::SingleQuotedString(_) => {
                r.shift_expr(Expression::String(parser.consume_string()?))
            }
            Token::Integer(_) => r.shift_expr(Expression::Integer(parser.consume_int()?)),
            Token::Float(_) => r.shift_expr(Expression::Float(parser.consume_float()?)),
            Token::Ident(_, Keyword::ASCII) => {
                r.shift_expr(parse_function(parser, Function::Ascii, false)?)
            }
            Token::Ident(_, Keyword::BIN) => {
                r.shift_expr(parse_function(parser, Function::Bin, false)?)
            }
            Token::Ident(_, Keyword::BIT_LENGTH) => {
                r.shift_expr(parse_function(parser, Function::BitLength, false)?)
            }
            Token::Ident(_, Keyword::CHAR_LENGTH) => {
                r.shift_expr(parse_function(parser, Function::CharacterLength, false)?)
            }

            Token::Ident(_, Keyword::CHARACTER_LENGTH) => {
                r.shift_expr(parse_function(parser, Function::CharacterLength, false)?)
            }

            Token::Ident(_, Keyword::CHR) => {
                r.shift_expr(parse_function(parser, Function::Chr, false)?)
            }
            Token::Ident(_, Keyword::CONCAT) => {
                r.shift_expr(parse_function(parser, Function::Concat, false)?)
            }
            Token::Ident(_, Keyword::CONCAT_WS) => {
                r.shift_expr(parse_function(parser, Function::ConcatWs, false)?)
            }
            Token::Ident(_, Keyword::ELT) => {
                r.shift_expr(parse_function(parser, Function::Elt, false)?)
            }
            Token::Ident(_, Keyword::EXPORT_SET) => {
                r.shift_expr(parse_function(parser, Function::ExportSet, false)?)
            }
            Token::Ident(_, Keyword::EXTRACTVALUE) => {
                r.shift_expr(parse_function(parser, Function::ExtractValue, false)?)
            }
            Token::Ident(_, Keyword::FIELD) => {
                r.shift_expr(parse_function(parser, Function::Field, false)?)
            }
            Token::Ident(_, Keyword::FIND_IN_SET) => {
                r.shift_expr(parse_function(parser, Function::FindInSet, false)?)
            }
            Token::Ident(_, Keyword::FORMAT) => {
                r.shift_expr(parse_function(parser, Function::Format, false)?)
            }
            Token::Ident(_, Keyword::FROM_BASE64) => {
                r.shift_expr(parse_function(parser, Function::FromBase64, false)?)
            }
            Token::Ident(_, Keyword::HEX) => {
                r.shift_expr(parse_function(parser, Function::Hex, false)?)
            }
            Token::Ident(_, Keyword::INSERT) => {
                r.shift_expr(parse_function(parser, Function::Insert, false)?)
            }
            Token::Ident(_, Keyword::INSTR) => {
                r.shift_expr(parse_function(parser, Function::InStr, false)?)
            }
            Token::Ident(_, Keyword::LCASE) => {
                r.shift_expr(parse_function(parser, Function::LCase, false)?)
            }
            Token::Ident(_, Keyword::LEFT) => {
                r.shift_expr(parse_function(parser, Function::Left, false)?)
            }
            Token::Ident(_, Keyword::LENGTH) => {
                r.shift_expr(parse_function(parser, Function::Length, false)?)
            }
            Token::Ident(_, Keyword::LENGTHB) => {
                r.shift_expr(parse_function(parser, Function::LengthB, false)?)
            }
            Token::Ident(_, Keyword::LOAD_FILE) => {
                r.shift_expr(parse_function(parser, Function::LoadFile, false)?)
            }
            Token::Ident(_, Keyword::LOCATE) => {
                r.shift_expr(parse_function(parser, Function::Locate, false)?)
            }
            Token::Ident(_, Keyword::LOWER) => {
                r.shift_expr(parse_function(parser, Function::Lower, false)?)
            }
            Token::Ident(_, Keyword::LPAD) => {
                r.shift_expr(parse_function(parser, Function::LPad, false)?)
            }
            Token::Ident(_, Keyword::LTRIM) => {
                r.shift_expr(parse_function(parser, Function::LTrim, false)?)
            }
            Token::Ident(_, Keyword::MAKE_SET) => {
                r.shift_expr(parse_function(parser, Function::MakeSet, false)?)
            }
            Token::Ident(_, Keyword::MID) => {
                r.shift_expr(parse_function(parser, Function::Mid, false)?)
            }
            Token::Ident(_, Keyword::NATURAL_SORT_KEY) => {
                r.shift_expr(parse_function(parser, Function::NaturalSortkey, false)?)
            }
            Token::Ident(_, Keyword::OCTET_LENGTH) => {
                r.shift_expr(parse_function(parser, Function::OctetLength, false)?)
            }
            Token::Ident(_, Keyword::ORD) => {
                r.shift_expr(parse_function(parser, Function::Ord, false)?)
            }
            Token::Ident(_, Keyword::POSITION) => {
                r.shift_expr(parse_function(parser, Function::Position, false)?)
            }
            Token::Ident(_, Keyword::QUOTE) => {
                r.shift_expr(parse_function(parser, Function::Quote, false)?)
            }
            Token::Ident(_, Keyword::REPEAT) => {
                r.shift_expr(parse_function(parser, Function::Repeat, false)?)
            }
            Token::Ident(_, Keyword::REPLACE) => {
                r.shift_expr(parse_function(parser, Function::Replace, false)?)
            }
            Token::Ident(_, Keyword::REVERSE) => {
                r.shift_expr(parse_function(parser, Function::Reverse, false)?)
            }
            Token::Ident(_, Keyword::RIGHT) => {
                r.shift_expr(parse_function(parser, Function::Right, false)?)
            }
            Token::Ident(_, Keyword::RPAD) => {
                r.shift_expr(parse_function(parser, Function::RPad, false)?)
            }
            Token::Ident(_, Keyword::RTRIM) => {
                r.shift_expr(parse_function(parser, Function::RTrim, false)?)
            }
            Token::Ident(_, Keyword::SOUNDEX) => {
                r.shift_expr(parse_function(parser, Function::SoundEx, false)?)
            }
            Token::Ident(_, Keyword::SPACE) => {
                r.shift_expr(parse_function(parser, Function::Space, false)?)
            }
            Token::Ident(_, Keyword::STRCMP) => {
                r.shift_expr(parse_function(parser, Function::StrCmp, false)?)
            }
            Token::Ident(_, Keyword::SUBSTR) => {
                r.shift_expr(parse_function(parser, Function::SubStr, false)?)
            }
            Token::Ident(_, Keyword::SUBSTRING) => {
                r.shift_expr(parse_function(parser, Function::SubStr, false)?)
            }
            Token::Ident(_, Keyword::SUBSTRING_INDEX) => {
                r.shift_expr(parse_function(parser, Function::SubStringIndex, false)?)
            }
            Token::Ident(_, Keyword::TO_BASE64) => {
                r.shift_expr(parse_function(parser, Function::ToBase64, false)?)
            }
            Token::Ident(_, Keyword::TO_CHAR) => {
                r.shift_expr(parse_function(parser, Function::ToChar, false)?)
            }
            Token::Ident(_, Keyword::UCASE) => {
                r.shift_expr(parse_function(parser, Function::UCase, false)?)
            }
            Token::Ident(_, Keyword::UNCOMPRESSED_LENGTH) => {
                r.shift_expr(parse_function(parser, Function::UncompressedLength, false)?)
            }
            Token::Ident(_, Keyword::UNHEX) => {
                r.shift_expr(parse_function(parser, Function::UnHex, false)?)
            }
            Token::Ident(_, Keyword::UPDATEXML) => {
                r.shift_expr(parse_function(parser, Function::UpdateXml, false)?)
            }
            Token::Ident(_, Keyword::UPPER) => {
                r.shift_expr(parse_function(parser, Function::Upper, false)?)
            }
            Token::Ident(_, Keyword::SFORMAT) => {
                r.shift_expr(parse_function(parser, Function::SFormat, false)?)
            }
            Token::Ident(_, Keyword::CURRENT_TIMESTAMP) => {
                r.shift_expr(parse_function(parser, Function::CurrentTimestamp, true)?)
            }
            Token::Ident(_, Keyword::COUNT) => {
                r.shift_expr(parse_function(parser, Function::Count, false)?)
            }
            Token::Ident(_, Keyword::JSON_EXTRACT) => {
                r.shift_expr(parse_function(parser, Function::JsonExtract, false)?)
            }
            Token::Ident(_, Keyword::IFNULL) => {
                r.shift_expr(parse_function(parser, Function::IfNull, false)?)
            }
            Token::Ident(_, Keyword::EXISTS) => {
                r.shift_expr(parse_function(parser, Function::Exists, false)?)
            }
            Token::Ident(_, k) if !k.reserved() => {
                let mut parts = vec![parser.consume_plain_identifier()?];
                loop {
                    if parser.skip_token(Token::Period).is_none() {
                        break;
                    }
                    parts.push(parser.consume_plain_identifier()?);
                }
                r.shift_expr(Expression::Identifier(parts))
            }
            Token::QuestionMark => {
                let arg = parser.arg;
                parser.arg += 1;
                r.shift_expr(Expression::Arg((
                    arg,
                    parser.consume_token(Token::QuestionMark)?,
                )))
            }
            Token::LParen => {
                parser.consume_token(Token::LParen)?;
                let ans = parse_expression_outer(parser)?;
                parser.consume_token(Token::RParen)?;
                r.shift_expr(ans)
            }
            _ => break,
        };
        if let Err(e) = e {
            parser.error(e.to_string())?;
        }
    }

    if r.reduce(99999).is_err() {
        parser.error("Expected expression")
    } else if r.stack.len() != 1 {
        parser.error("ICE reducer error 1")
    } else if let Some(ReduceMember::Expression(e)) = r.stack.pop() {
        Ok(e)
    } else {
        parser.error("ICE reducer error 2")
    }
}

pub(crate) fn parse_expression_outer<'a>(
    parser: &mut Parser<'a>,
) -> Result<Expression<'a>, ParseError> {
    if matches!(parser.token, Token::Ident(_, Keyword::SELECT)) {
        Ok(Expression::Subquery(Box::new(parse_select(parser)?)))
    } else {
        parse_expression(parser, false)
    }
}
