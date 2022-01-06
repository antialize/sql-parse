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

use std::{borrow::Cow, fmt::Write};

use crate::{
    keywords::Keyword,
    lexer::Token,
    parser::{ParseError, Parser, SingleQuotedString},
    Span, Spanned,
};

// #[derive(Debug, Clone)]
// pub enum Literal<'a> {

// }

// impl<'a> std::fmt::Display for Literal<'a> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Literal::String((v, _)) => write!(f, "COMMENT {}", SingleQuotedString(v.as_ref())),
//             Literal::Null(_) => f.write_str("NULL"),
//             Literal::Integer((v, _)) => write!(f, "{}", v),
//             Literal::Float((v, _)) => write!(f, "{}", v),
//             Literal::CurrentTimestamp(_) => f.write_str("CURRENT_TIMESTAMP()"),
//             Literal::Identifier(p) => {
//                 for (i, (p, _)) in p.iter().enumerate() {
//                     if i != 0 {
//                         f.write_char('.')?;
//                     }
//                     f.write_str(p)?;
//                 }
//                 Ok(())
//             }
//             Literal::Arg(_) => 
//                 f.write_char('?'),
//         }
//     }
// }

// impl<'a> Spanned for Literal<'a> {
//     fn span(&self) -> Span {
//         match &self {
//             Literal::Null(_) => todo!(),
//             Literal::String(_) => todo!(),
//             Literal::Integer(_) => todo!(),
//             Literal::Float(_) => todo!(),
//             Literal::CurrentTimestamp(_) => todo!(),
//             Literal::Identifier(_) => todo!(),
//             _ => todo!(),
//         }
//     }
// }


#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Or(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Xor(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    And(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Not(Box<Expression<'a>>, Span),
    Eq(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    NullSafeEq(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    GtEq(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Gt(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    LtEq(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Lt(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Neq(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Is(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Like(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Regexp(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    In(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    BitOr(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    BitAnd(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    ShiftLeft(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    ShiftRight(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Plus(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Minus(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Multiplication(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Div(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Division(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    Mod(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    BitXor(Box<Expression<'a>>, Box<Expression<'a>>, Span),
    UnaryMinus(Box<Expression<'a>>, Span),
    Binary(Box<Expression<'a>>, Span),
    Collate(Box<Expression<'a>>, Span),
    Null(Span),
    String((Cow<'a, str>, Span)),
    Integer((u64, Span)),
    Float((f64, Span)),
    CurrentTimestamp(Span),
    Identifier(Vec<(&'a str, Span)>),
    Arg((usize, Span))
}

impl<'a> std::fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => todo!(),
        }
    }
}

impl<'a> Spanned for Expression<'a> {
    fn span(&self) -> Span {
        match &self {
            _ => todo!()
        }
    }
}

pub(crate) fn parse_expression_bottom<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    match &parser.token {
        Token::Ident(_, Keyword::NULL) => Ok(Expression::Null(parser.consume_keyword(Keyword::NULL)?)),
        Token::SingleQuotedString(_) => Ok(Expression::String(parser.consume_string()?)),
        Token::Integer(_) => Ok(Expression::Integer(parser.consume_int()?)),
        Token::Float(_) => Ok(Expression::Float(parser.consume_float()?)),
        Token::Ident(_, kw @ Keyword::CURRENT_TIMESTAMP) => {
            let kw = *kw;
            let s = parser.consume_keyword(kw)?;
            if parser.skip_token(Token::LParen).is_some() {
                parser.consume_token(Token::RParen)?;
            }
            Ok(Expression::CurrentTimestamp(s))
        }
        Token::Ident(_, k) if ! k.reserved() => {
            let mut parts = vec![parser.consume_plain_identifier()?];
            loop {
                if parser.skip_token(Token::Period).is_none() {
                    break;
                }
                parts.push(parser.consume_plain_identifier()?);
            };
            Ok(Expression::Identifier(parts))
        }
        Token::QuestionMark => { 
            let arg = parser.arg;
            parser.arg += 1;
            Ok(Expression::Arg((arg, parser.consume_token(Token::QuestionMark)?)))
        }
        Token::LParen => {
            parser.consume_token(Token::LParen)?;
            let ans = parse_expression(parser)?;
            parser.consume_token(Token::RParen)?;
            Ok(ans)
        }
        _ => parser.expected_failure("expression"),
    }
}

pub(crate) fn parse_expression_interval<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    //TODO
    Ok(parse_expression_bottom(parser)?)
}

pub(crate) fn parse_expression_binary<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    if let Some(op) = parser.skip_keyword(Keyword::BINARY) {
        Ok(Expression::Binary(Box::new(parse_expression_binary(parser)?), op))
    } else if let Some(op) = parser.skip_keyword(Keyword::COLLATE) {
        Ok(Expression::Collate(Box::new(parse_expression_binary(parser)?), op))
    } else {
        parse_expression_interval(parser)
    }
}

pub(crate) fn parse_expression_bang<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    if let Some(op) = parser.skip_token(Token::ExclamationMark) {
        Ok(Expression::Not(Box::new(parse_expression_bang(parser)?), op))
    } else {
        parse_expression_binary(parser)
    }
}

pub(crate) fn parse_expression_unary_minus<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    if let Some(op) = parser.skip_token(Token::Minus) {
        Ok(Expression::UnaryMinus(Box::new(parse_expression_unary_minus(parser)?), op))
    } else {
        parse_expression_bang(parser)
    }
}


pub(crate) fn parse_expression_bitxor<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_unary_minus(parser)?;
    // TODO
    Ok(ans)
}

pub(crate) fn parse_expression_mult<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_bitxor(parser)?;
    // TODO
    Ok(ans)
}

pub(crate) fn parse_expression_add<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_mult(parser)?;
    // TODO
    Ok(ans)
}


pub(crate) fn parse_expression_shift<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_add(parser)?;
    // TODO
    Ok(ans)
}


pub(crate) fn parse_expression_bitand<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_shift(parser)?;
    loop {
        if let Some(op) = parser.skip_token(Token::Ampersand) {
            ans = Expression::BitAnd(Box::new(ans), Box::new(parse_expression_shift(parser)?), op)
        } else {
            break;
        }
    }
    Ok(ans)
}

pub(crate) fn parse_expression_bitor<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_bitand(parser)?;
    loop {
        if let Some(op) = parser.skip_token(Token::Pipe) {
            ans = Expression::BitOr(Box::new(ans), Box::new(parse_expression_bitand(parser)?), op)
        } else {
            break;
        }
    }
    Ok(ans)
}

pub(crate) fn parse_expression_compare<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_bitor(parser)?;
    loop {
        match &parser.token {
            Token::Eq => {
                let op = parser.consume_token(Token::Eq)?;
                ans = Expression::Eq(Box::new(ans), Box::new(parse_expression_bitor(parser)?), op);
            },
            Token::Spaceship => {
                let op = parser.consume_token(Token::Spaceship)?;
                ans = Expression::NullSafeEq(Box::new(ans), Box::new(parse_expression_bitor(parser)?), op);
            },
            Token::GtEq => {
                let op = parser.consume_token(Token::GtEq)?;
                ans = Expression::GtEq(Box::new(ans), Box::new(parse_expression_bitor(parser)?), op);
            },
            Token::Gt => {
                let op = parser.consume_token(Token::Gt)?;
                ans = Expression::Gt(Box::new(ans), Box::new(parse_expression_bitor(parser)?), op);
            },
            Token::LtEq => {
                let op = parser.consume_token(Token::LtEq)?;
                ans = Expression::LtEq(Box::new(ans), Box::new(parse_expression_bitor(parser)?), op);
            },
            Token::Lt => {
                let op = parser.consume_token(Token::Lt)?;
                ans = Expression::Lt(Box::new(ans), Box::new(parse_expression_bitor(parser)?), op);
            },
            Token::Neq => {
                let op = parser.consume_token(Token::Neq)?;
                ans = Expression::Neq(Box::new(ans), Box::new(parse_expression_bitor(parser)?), op);
            }
            // TODO IS boolean_value
            // TODO expr REGEXP pat, expr RLIKE pat
            // TODO expr IN (value,...)
            // TODO expr LIKE pat [ESCAPE 'escape_char']
            // expr NOT LIKE pat [ESCAPE 'escape_char']
            _ => break
        }
        
    }
    Ok(ans)
}

pub(crate) fn parse_expression_case<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    // TODO
    parse_expression_compare(parser)
}

pub(crate) fn parse_expression_not<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    if let Some(op) = parser.skip_keyword(Keyword::NOT) {
        Ok(Expression::Not(Box::new(parse_expression_not(parser)?), op))
    } else {
        parse_expression_case(parser)
    }
}

pub(crate) fn parse_expression_and<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_not(parser)?;
    loop {
        if let Some(op) = parser.skip_keyword(Keyword::AND) {
            ans = Expression::And(Box::new(ans), Box::new(parse_expression_not(parser)?), op);
        } else if let Some(op) = parser.skip_token(Token::DoubleAmpersand) {
            ans = Expression::And(Box::new(ans), Box::new(parse_expression_not(parser)?), op);
        } else {
            break;
        }
    }
    Ok(ans)
}

pub(crate) fn parse_expression_xor<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_and(parser)?;
    loop {
        if let Some(op) = parser.skip_keyword(Keyword::XOR) {
            ans = Expression::Xor(Box::new(ans), Box::new(parse_expression_and(parser)?), op)
        } else {
            break;
        }
    }
    Ok(ans)
}

pub(crate) fn parse_expression_or<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    let mut ans = parse_expression_xor(parser)?;
    loop {
        if let Some(op) = parser.skip_keyword(Keyword::OR) {
            ans = Expression::And(Box::new(ans), Box::new(parse_expression_xor(parser)?), op);
        } else if let Some(op) = parser.skip_token(Token::DoublePipe) {
            ans = Expression::And(Box::new(ans), Box::new(parse_expression_xor(parser)?), op);
        } else {
            break;
        }
    }
    Ok(ans)
}


pub(crate) fn parse_expression<'a>(parser: &mut Parser<'a>) -> Result<Expression<'a>, ParseError> {
    parse_expression_or(parser)
}
