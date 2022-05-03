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

use alloc::{borrow::Cow, fmt::Write, format, string::String, vec::Vec};

use crate::{
    issue::Issue,
    keywords::Keyword,
    lexer::{Lexer, Token},
    Identifier, ParseOptions, SString, Span, Spanned,
};

#[derive(Debug)]
pub(crate) enum ParseError {
    Unrecovered,
}

pub(crate) struct Parser<'a, 'b> {
    pub(crate) token: Token<'a>,
    pub(crate) span: Span,
    pub(crate) lexer: Lexer<'a>,
    pub(crate) issues: &'b mut Vec<Issue>,
    pub(crate) arg: usize,
    pub(crate) delimiter: Token<'a>,
    pub(crate) options: &'b ParseOptions,
    pub(crate) permit_compound_statements: bool,
}

pub(crate) fn decode_single_quoted_string(s: &str) -> Cow<'_, str> {
    if !s.contains('\'') && !s.contains('\\') {
        s.into()
    } else {
        let mut r = String::new();
        let mut chars = s.chars();
        loop {
            match chars.next() {
                None => break,
                Some('\'') => {
                    chars.next();
                    r.push('\'');
                }
                Some(c) => r.push(c),
            }
        }
        r.into()
    }
}

pub(crate) fn decode_double_quoted_string(s: &str) -> Cow<'_, str> {
    if !s.contains('"') && !s.contains('\\') {
        s.into()
    } else {
        let mut r = String::new();
        let mut chars = s.chars();
        loop {
            match chars.next() {
                None => break,
                Some('\'') => {
                    chars.next();
                    r.push('\'');
                }
                Some(c) => r.push(c),
            }
        }
        r.into()
    }
}

pub(crate) struct SingleQuotedString<'a>(pub(crate) &'a str);

impl<'a> alloc::fmt::Display for SingleQuotedString<'a> {
    fn fmt(&self, f: &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result {
        f.write_char('\'')?;
        for c in self.0.chars() {
            if c == '\'' {
                f.write_char('\'')?;
            }
            f.write_char(c)?;
        }
        f.write_char('\'')
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    pub(crate) fn new(src: &'a str, issues: &'b mut Vec<Issue>, options: &'b ParseOptions) -> Self {
        let mut lexer = Lexer::new(src);
        let (token, span) = lexer.next_token();
        Self {
            token,
            span,
            lexer,
            issues,
            arg: 0,
            delimiter: Token::SemiColon,
            options,
            permit_compound_statements: false,
        }
    }

    pub(crate) fn recover(
        &mut self,
        success: impl Fn(&Token<'a>) -> bool,
        fail: impl Fn(&Token<'a>) -> bool,
    ) -> Result<(), ParseError> {
        let mut brackets = Vec::new();
        loop {
            match &self.token {
                t if brackets.is_empty() && success(t) => return Ok(()),
                Token::Eof => return Err(ParseError::Unrecovered),
                t if t == &self.delimiter => return Err(ParseError::Unrecovered),
                t if brackets.is_empty() && fail(t) => return Err(ParseError::Unrecovered),
                Token::LParen => {
                    brackets.push(Token::LParen);
                    self.next();
                }
                Token::LBracket => {
                    brackets.push(Token::LBracket);
                    self.next();
                }
                Token::LBrace => {
                    brackets.push(Token::LBrace);
                    self.next();
                }
                Token::RBrace => {
                    self.next();
                    while let Some(v) = brackets.pop() {
                        if v == Token::LBrace {
                            break;
                        }
                    }
                }
                Token::RBracket => {
                    self.next();
                    while let Some(v) = brackets.pop() {
                        if v == Token::LBracket {
                            break;
                        }
                    }
                }
                Token::RParen => {
                    self.next();
                    while let Some(v) = brackets.pop() {
                        if v == Token::LParen {
                            break;
                        }
                    }
                }
                _ => self.next(),
            }
        }
    }

    pub(crate) fn recovered<T: Default>(
        &mut self,
        expected: &'static str,
        end: &impl Fn(&Token<'a>) -> bool,
        fun: impl FnOnce(&mut Self) -> Result<T, ParseError>,
    ) -> Result<T, ParseError> {
        let ans = match fun(self) {
            Ok(v) => v,
            Err(_) => {
                self.recover(end, |_| false)?;
                T::default()
            }
        };
        if !end(&self.token) {
            self.expected_error(expected);
            self.recover(end, |_| false)?;
        }
        Ok(ans)
    }

    pub(crate) fn read_from_stdin_and_next(&mut self) -> (&'a str, Span) {
        let stdin = self.lexer.read_from_stdin();
        let (token, span) = self.lexer.next_token();
        self.token = token;
        self.span = span;
        stdin
    }

    pub(crate) fn next(&mut self) {
        let (token, span) = self.lexer.next_token();
        self.token = token;
        self.span = span;
    }

    pub(crate) fn expected_error(&mut self, name: &'static str) {
        self.issues
            .push(Issue::err(format!("Expected '{}' here", name), &self.span));
    }

    pub(crate) fn expected_failure<T>(&mut self, name: &'static str) -> Result<T, ParseError> {
        self.expected_error(name);
        Err(ParseError::Unrecovered)
    }

    pub(crate) fn token_to_plain_identifier(
        &mut self,
        token: &Token<'a>,
        span: Span,
    ) -> Result<Identifier<'a>, ParseError> {
        match &token {
            Token::Ident(v, kw) => {
                let v = *v;
                if kw.reserved() {
                    self.issues.push(Issue::err(
                        format!("'{}' is a reserved identifier use `{}`", v, v),
                        &span,
                    ));
                } else if kw != &Keyword::QUOTED_IDENTIFIER
                    && self.options.warn_unquoted_identifiers
                {
                    self.issues.push(Issue::warn(
                        format!("identifiers should be quoted as `{}`", v),
                        &span,
                    ));
                }
                Ok(Identifier::new(v, span))
            }
            _ => self.expected_failure("identifier"),
        }
    }

    pub(crate) fn consume_plain_identifier(&mut self) -> Result<Identifier<'a>, ParseError> {
        match &self.token {
            Token::Ident(v, kw) => {
                let v = *v;
                if kw.reserved() {
                    self.issues.push(Issue::err(
                        format!("'{}' is a reserved identifier use `{}`", v, v),
                        &self.span,
                    ));
                } else if kw != &Keyword::QUOTED_IDENTIFIER
                    && self.options.warn_unquoted_identifiers
                {
                    self.issues.push(Issue::warn(
                        format!("identifiers should be quoted as `{}`", v),
                        &self.span,
                    ));
                }
                Ok(Identifier::new(v, self.consume()))
            }
            Token::DoubleQuotedString(v) if self.options.dialect.is_postgresql() => {
                Ok(Identifier::new(*v, self.consume()))
            }
            _ => self.expected_failure("identifier"),
        }
    }

    pub(crate) fn consume_keyword(&mut self, keyword: Keyword) -> Result<Span, ParseError> {
        match &self.token {
            Token::Ident(v, kw) if kw == &keyword => {
                if !v.chars().all(|c| c.is_ascii_uppercase())
                    && self.options.warn_none_capital_keywords
                {
                    self.issues.push(Issue::warn(
                        format!(
                            "keyword {} should be in ALL CAPS {}",
                            v,
                            v.to_ascii_uppercase()
                        ),
                        &self.span,
                    ));
                }
                Ok(self.consume())
            }
            _ => self.expected_failure(keyword.name()),
        }
    }

    pub(crate) fn consume_keywords(&mut self, keywords: &[Keyword]) -> Result<Span, ParseError> {
        let mut span = self.consume_keyword(keywords[0])?;
        for keyword in &keywords[1..] {
            span = self.consume_keyword(*keyword)?.join_span(&span);
        }
        Ok(span)
    }

    pub(crate) fn skip_keyword(&mut self, keyword: Keyword) -> Option<Span> {
        match &self.token {
            Token::Ident(_, kw) if kw == &keyword => Some(self.consume_keyword(keyword).unwrap()),
            _ => None,
        }
    }

    pub(crate) fn consume_token(&mut self, token: Token) -> Result<Span, ParseError> {
        if self.token != token {
            self.expected_failure(token.name())
        } else {
            Ok(self.consume())
        }
    }

    pub(crate) fn skip_token(&mut self, token: Token) -> Option<Span> {
        if self.token != token {
            None
        } else {
            Some(self.consume())
        }
    }

    pub(crate) fn consume(&mut self) -> Span {
        let span = self.span.clone();
        self.next();
        span
    }

    pub(crate) fn consume_string(&mut self) -> Result<SString<'a>, ParseError> {
        let (mut a, mut b) = match &self.token {
            Token::SingleQuotedString(v) => {
                let v = *v;
                let span = self.span.clone();
                self.next();
                (decode_single_quoted_string(v), span)
            }
            Token::DoubleQuotedString(v) => {
                let v = *v;
                let span = self.span.clone();
                self.next();
                (decode_double_quoted_string(v), span)
            }
            _ => self.expected_failure("string")?,
        };
        loop {
            match self.token {
                Token::SingleQuotedString(v) => {
                    b = b.join_span(&self.span);
                    a.to_mut().push_str(decode_single_quoted_string(v).as_ref());
                    self.next();
                }
                Token::DoubleQuotedString(v) => {
                    b = b.join_span(&self.span);
                    a.to_mut().push_str(decode_double_quoted_string(v).as_ref());
                    self.next();
                }
                _ => break,
            }
        }
        Ok(SString::new(a, b))
    }

    pub(crate) fn consume_int<T: core::str::FromStr + Default>(
        &mut self,
    ) -> Result<(T, Span), ParseError> {
        match &self.token {
            Token::Integer(v) => {
                let v = match v.parse() {
                    Ok(v) => v,
                    Err(_) => self.error("integer outside range").unwrap_or_default(),
                };
                let span = self.span.clone();
                self.next();
                Ok((v, span))
            }
            _ => self.expected_failure("integer"),
        }
    }

    pub(crate) fn consume_float<T: core::str::FromStr + Default>(
        &mut self,
    ) -> Result<(T, Span), ParseError> {
        match &self.token {
            Token::Float(v) => {
                let v = match v.parse() {
                    Ok(v) => v,
                    Err(_) => self.error("float outside range").unwrap_or_default(),
                };
                let span = self.span.clone();
                self.next();
                Ok((v, span))
            }
            _ => self.expected_failure("float"),
        }
    }

    pub(crate) fn error<T>(&mut self, message: impl Into<String>) -> Result<T, ParseError> {
        self.issues.push(Issue::err(message, &self.span));
        Err(ParseError::Unrecovered)
    }

    pub(crate) fn ice<T>(&mut self, file: &'static str, line: u32) -> Result<T, ParseError> {
        self.error(format!("Internal compiler error at {}:{}", file, line))
    }

    pub(crate) fn todo<T>(&mut self, file: &'static str, line: u32) -> Result<T, ParseError> {
        self.error(format!("Not yet implemented at {}:{}", file, line))
    }
}
