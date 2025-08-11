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

use crate::{keywords::Keyword, Span};

/// SQL Token enumeration
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Token<'a> {
    Ampersand,
    At,
    Backslash,
    Caret,
    Colon,
    Comma,
    Div,
    DoubleColon,
    DoubleExclamationMark,
    DoubleAmpersand,
    DoublePipe,
    DoubleDollar,
    Eq,
    ExclamationMark,
    Float(&'a str),
    Gt,
    GtEq,
    Ident(&'a str, Keyword),
    Integer(&'a str),
    Invalid,
    LBrace,
    LBracket,
    LParen,
    Lt,
    LtEq,
    Minus,
    Mod,
    Mul,
    Neq,
    Period,
    Pipe,
    Plus,
    QuestionMark,
    RArrow,
    RBrace,
    RBracket,
    RParen,
    SemiColon,
    Sharp,
    ShiftLeft,
    ShiftRight,
    SingleQuotedString(&'a str),
    DoubleQuotedString(&'a str),
    Spaceship,
    Tilde,
    PercentS,
    DollarArg(usize),
    AtAtGlobal,
    AtAtSession,
    Eof,
}

impl<'a> Token<'a> {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            Token::Ampersand => "'&'",
            Token::At => "'@'",
            Token::Backslash => "'\\'",
            Token::Caret => "'^'",
            Token::Colon => "':'",
            Token::Comma => "','",
            Token::Div => "'/'",
            Token::DoubleColon => "'::'",
            Token::DoubleExclamationMark => "'!!'",
            Token::DoublePipe => "'||'",
            Token::DoubleAmpersand => "'&&'",
            Token::Eq => "'='",
            Token::ExclamationMark => "'!'",
            Token::Float(_) => "Float",
            Token::Gt => "'>'",
            Token::GtEq => "'>='",
            Token::Ident(_, Keyword::NOT_A_KEYWORD) => "Identifier",
            Token::Ident(_, Keyword::QUOTED_IDENTIFIER) => "QuotedIdentifier",
            Token::Ident(_, kw) => kw.name(),
            Token::Integer(_) => "Integer",
            Token::Invalid => "Invalid",
            Token::LBrace => "'{'",
            Token::LBracket => "'['",
            Token::LParen => "'('",
            Token::Lt => "'<'",
            Token::LtEq => "'<='",
            Token::Minus => "'-'",
            Token::Mod => "'%'",
            Token::Mul => "'*'",
            Token::Neq => "'!='",
            Token::Period => "'.'",
            Token::Pipe => "'|'",
            Token::Plus => "'+'",
            Token::QuestionMark => "'?'",
            Token::RArrow => "'=>'",
            Token::RBrace => "'}'",
            Token::RBracket => "']'",
            Token::RParen => "')'",
            Token::SemiColon => "';'",
            Token::Sharp => "'#'",
            Token::ShiftLeft => "'>>'",
            Token::ShiftRight => "'<<'",
            Token::DoubleDollar => "'$$'",
            Token::DollarArg(v) if *v == 1 => "'$1'",
            Token::DollarArg(v) if *v == 2 => "'$2'",
            Token::DollarArg(v) if *v == 3 => "'$3'",
            Token::DollarArg(v) if *v == 4 => "'$4'",
            Token::DollarArg(v) if *v == 5 => "'$5'",
            Token::DollarArg(v) if *v == 6 => "'$6'",
            Token::DollarArg(v) if *v == 7 => "'$7'",
            Token::DollarArg(v) if *v == 8 => "'$8'",
            Token::DollarArg(v) if *v == 9 => "'$9'",
            Token::DollarArg(_) => "'$i'",
            Token::SingleQuotedString(_) => "String",
            Token::DoubleQuotedString(_) => "String",
            Token::Spaceship => "'<=>'",
            Token::Tilde => "'~'",
            Token::PercentS => "'%s'",
            Token::AtAtGlobal => "@@GLOBAL",
            Token::AtAtSession => "@@SESSION",
            Token::Eof => "EndOfFile",
        }
    }
}
pub(crate) struct Lexer<'a> {
    src: &'a str,
    chars: core::iter::Peekable<core::str::CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
        }
    }

    pub(crate) fn s(&self, span: Span) -> &'a str {
        core::str::from_utf8(&self.src.as_bytes()[span]).unwrap()
    }

    fn simple_literal(&mut self, start: usize) -> Token<'a> {
        let end = loop {
            match self.chars.peek() {
                Some((_, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')) => {
                    self.chars.next();
                }
                Some((i, _)) => break *i,
                None => break self.src.len(),
            }
        };
        let s = self.s(start..end);
        let ss = s.to_ascii_uppercase();
        Token::Ident(s, ss.as_str().into())
    }

    /// Simulate reading from standard input after a statement like `COPY ... FROM STDIN;`.
    /// First skips space characters and optionally one NL.
    /// Then consumes until NL '\' '.' NL is encountered, or until EOF.
    /// The trailing '\' '.' NL is consumed but not returned.
    pub fn read_from_stdin(&mut self) -> (&'a str, Span) {
        // Skip optional spaces.
        while self
            .chars
            .peek()
            .filter(|(_, c)| *c != '\n' && c.is_ascii_whitespace())
            .is_some()
        {
            self.chars.next().unwrap();
        }
        let start = match self.chars.peek() {
            Some((i, '\n')) => i + 1,
            Some((i, _)) => *i,
            None => {
                let span = self.src.len()..self.src.len();
                return (self.s(span.clone()), span);
            }
        };
        while let Some((i, c)) = self.chars.next() {
            if c != '\n' {
                continue;
            }
            if !matches!(self.chars.peek(), Some((_, '\\'))) {
                continue;
            }
            self.chars.next().unwrap();
            if !matches!(self.chars.peek(), Some((_, '.'))) {
                continue;
            }
            self.chars.next().unwrap();
            if matches!(self.chars.peek(), Some((_, '\n'))) {
                // Data ends with NL '\' '.' NL.
                self.chars.next().unwrap();
            } else if self.chars.peek().is_some() {
                continue;
            } else {
                // Data ends with NL '\' '.' without an extra NL,
                // which is fine.
            }
            // `i` is the character index of the first '\n',
            // so the data ends at character index i + 1.
            let span = start..(i + 1);
            return (self.s(span.clone()), span);
        }
        // Data ends at EOF without NL '\' '.' [NL].
        let span = start..self.src.len();
        (self.s(span.clone()), span)
    }

    pub fn next_token(&mut self) -> (Token<'a>, Span) {
        loop {
            let (start, c) = match self.chars.next() {
                Some(v) => v,
                None => {
                    return (Token::Eof, self.src.len()..self.src.len());
                }
            };
            let t = match c {
                ' ' | '\t' | '\n' | '\r' => continue,
                '?' => Token::QuestionMark,
                ';' => Token::SemiColon,
                '\\' => Token::Backslash,
                '[' => Token::LBracket,
                ']' => Token::RBracket,
                '&' => match self.chars.peek() {
                    Some((_, '&')) => {
                        self.chars.next();
                        Token::DoubleAmpersand
                    }
                    _ => Token::Ampersand,
                },
                '^' => Token::Caret,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '(' => Token::LParen,
                ')' => Token::RParen,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '*' => Token::Mul,
                '%' => match self.chars.peek() {
                    Some((_, 's')) => {
                        self.chars.next();
                        Token::PercentS
                    }
                    _ => Token::Mod,
                },
                '#' => Token::Sharp,
                '@' => match self.chars.peek() {
                    Some((_, '@')) => {
                        self.chars.next();
                        #[allow(clippy::never_loop)]
                        match self.chars.peek() {
                            Some((_, 's' | 'S')) => loop {
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'e' | 'E'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 's' | 'S'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 's' | 'S'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'i' | 'I'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'o' | 'O'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'n' | 'N'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                break Token::AtAtSession;
                            },
                            Some((_, 'g' | 'G')) => loop {
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'l' | 'L'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'o' | 'O'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'b' | 'B'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'a' | 'A'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                if !matches!(self.chars.peek(), Some((_, 'l' | 'L'))) {
                                    break Token::Invalid;
                                }
                                self.chars.next();
                                break Token::AtAtGlobal;
                            },
                            _ => Token::Invalid,
                        }
                    }
                    _ => Token::At,
                },
                '~' => Token::Tilde,
                ':' => match self.chars.peek() {
                    Some((_, ':')) => {
                        self.chars.next();
                        Token::DoubleColon
                    }
                    _ => Token::Colon,
                },
                '$' => match self.chars.peek() {
                    Some((_, '$')) => {
                        self.chars.next();
                        Token::DoubleDollar
                    }
                    Some((_, '1'..='9')) => {
                        let mut v = self.chars.peek().unwrap().1.to_digit(10).unwrap() as usize;
                        self.chars.next();
                        while matches!(self.chars.peek(), Some((_, '0'..='9'))) {
                            v = v * 10
                                + self.chars.peek().unwrap().1.to_digit(10).unwrap() as usize;
                            self.chars.next();
                        }
                        Token::DollarArg(v)
                    }
                    _ => Token::Invalid,
                },
                '=' => match self.chars.peek() {
                    Some((_, '>')) => {
                        self.chars.next();
                        Token::RArrow
                    }
                    _ => Token::Eq,
                },
                '!' => match self.chars.peek() {
                    Some((_, '=')) => {
                        self.chars.next();
                        Token::Neq
                    }
                    Some((_, '!')) => {
                        self.chars.next();
                        Token::DoubleExclamationMark
                    }
                    _ => Token::ExclamationMark,
                },
                '<' => match self.chars.peek() {
                    Some((_, '=')) => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some((_, '>')) => {
                                self.chars.next();
                                Token::Spaceship
                            }
                            _ => Token::LtEq,
                        }
                    }
                    Some((_, '>')) => {
                        self.chars.next();
                        Token::Neq
                    }
                    Some((_, '<')) => {
                        self.chars.next();
                        Token::ShiftLeft
                    }
                    _ => Token::Lt,
                },
                '>' => match self.chars.peek() {
                    Some((_, '=')) => {
                        self.chars.next();
                        Token::GtEq
                    }
                    Some((_, '>')) => {
                        self.chars.next();
                        Token::ShiftRight
                    }
                    _ => Token::Gt,
                },
                '|' => match self.chars.peek() {
                    Some((_, '|')) => {
                        self.chars.next();
                        Token::DoublePipe
                    }
                    _ => Token::Pipe,
                },
                '-' => match self.chars.peek() {
                    Some((_, '-')) => {
                        while !matches!(self.chars.next(), Some((_, '\r' | '\n')) | None) {}
                        continue;
                    }
                    _ => Token::Minus,
                },
                '/' => match self.chars.peek() {
                    Some((_, '*')) => {
                        self.chars.next();
                        let ok = loop {
                            match self.chars.next() {
                                Some((_, '*')) => {
                                    if matches!(self.chars.peek(), Some((_, '/'))) {
                                        self.chars.next();
                                        break true;
                                    }
                                }
                                Some(_) => (),
                                None => break false,
                            }
                        };
                        if ok {
                            continue;
                        } else {
                            Token::Invalid
                        }
                    }
                    Some((_, '/')) => {
                        while !matches!(self.chars.next(), Some((_, '\r' | '\n')) | None) {}
                        continue;
                    }
                    _ => Token::Div,
                },
                'x' | 'X' => match self.chars.peek() {
                    Some((_, '\'')) => {
                        todo!("Hex literal")
                    }
                    _ => self.simple_literal(start),
                },
                '_' | 'a'..='z' | 'A'..='Z' => self.simple_literal(start),
                '`' => {
                    while matches!(
                        self.chars.peek(),
                        Some((_, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' | '-'))
                    ) {
                        self.chars.next();
                    }
                    match self.chars.peek() {
                        Some((i, '`')) => {
                            let i = *i;
                            self.chars.next();
                            Token::Ident(self.s(start + 1..i), Keyword::QUOTED_IDENTIFIER)
                        }
                        _ => Token::Invalid,
                    }
                }
                '\'' => loop {
                    match self.chars.next() {
                        Some((_, '\\')) => {
                            self.chars.next();
                        }
                        Some((i, '\'')) => match self.chars.peek() {
                            Some((_, '\'')) => {
                                self.chars.next();
                            }
                            _ => break Token::SingleQuotedString(self.s(start + 1..i)),
                        },
                        Some((_, _)) => (),
                        None => break Token::Invalid,
                    }
                },
                '"' => loop {
                    match self.chars.next() {
                        Some((_, '\\')) => {
                            self.chars.next();
                        }
                        Some((i, '"')) => match self.chars.peek() {
                            Some((_, '"')) => {
                                self.chars.next();
                            }
                            _ => break Token::DoubleQuotedString(self.s(start + 1..i)),
                        },
                        Some((_, _)) => (),
                        None => break Token::Invalid,
                    }
                },
                '0'..='9' => loop {
                    match self.chars.peek() {
                        Some((_, '0'..='9')) => {
                            self.chars.next();
                        }
                        Some((_, '.')) => {
                            self.chars.next();
                            break loop {
                                match self.chars.peek() {
                                    Some((_, '0'..='9')) => {
                                        self.chars.next();
                                    }
                                    Some((i, _)) => {
                                        let i = *i;
                                        break Token::Float(self.s(start..i));
                                    }
                                    None => break Token::Float(self.s(start..self.src.len())),
                                }
                            };
                        }
                        Some((i, _)) => {
                            let i = *i;
                            break Token::Integer(self.s(start..i));
                        }
                        None => break Token::Integer(self.s(start..self.src.len())),
                    }
                },
                '.' => match self.chars.peek() {
                    Some((_, '0'..='9')) => loop {
                        match self.chars.peek() {
                            Some((_, '0'..='9')) => {
                                self.chars.next();
                            }
                            Some((i, _)) => {
                                let i = *i;
                                break Token::Float(self.s(start..i));
                            }
                            None => break Token::Float(self.s(start..self.src.len())),
                        }
                    },
                    _ => Token::Period,
                },
                _ => Token::Invalid,
            };

            let end = match self.chars.peek() {
                Some((i, _)) => *i,
                None => self.src.len(),
            };
            return (t, start..end);

            // // string

            // '\'' => {
            //     let value = self.tokenize_single_quoted_string(chars)?;
            //     Ok(Some(Token::SingleQuotedString { value, span }))
            // }

            // // numbers and period
            // '0'..='9' | '.' => {
            //     let mut value = peeking_take_while(chars, |ch| matches!(ch, '0'..='9'));

            //     // match binary literal that starts with 0x
            //     if value == "0" && chars.peek().map(|(_, c)| c) == Some(&'x') {
            //         chars.next();
            //         let value = peeking_take_while(
            //             chars,
            //             |ch| matches!(ch, '0'..='9' | 'A'..='F' | 'a'..='f'),
            //         );
            //         return Ok(Some(Token::HexStringLiteral { value, span }));
            //     }

            //     // match one period
            //     if let Some((_, '.')) = chars.peek() {
            //         value.push('.');
            //         chars.next();
            //     }
            //     value += &peeking_take_while(chars, |ch| matches!(ch, '0'..='9'));

            //     // No number -> Token::Period
            //     if value == "." {
            //         return Ok(Some(Token::Period { span }));
            //     }

            //     let long = if let Some((_, 'L')) = chars.peek() {
            //         chars.next();
            //         true
            //     } else {
            //         false
            //     };
            //     Ok(Some(Token::Number { value, long, span }))
            // }
            // // punctuation

            // // operators
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token<'a>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}
