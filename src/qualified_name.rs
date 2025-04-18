use alloc::vec::Vec;

use crate::{
    lexer::Token,
    parser::{ParseError, Parser},
    Identifier, Span, Spanned,
};

#[derive(Clone, Debug)]
pub struct QualifiedName<'a> {
    pub prefix: Vec<(Identifier<'a>, Span)>,
    pub identifier: Identifier<'a>,
}

impl Spanned for QualifiedName<'_> {
    fn span(&self) -> Span {
        self.identifier.join_span(&self.prefix)
    }
}

pub(crate) fn parse_qualified_name<'a>(
    parser: &mut Parser<'a, '_>,
) -> Result<QualifiedName<'a>, ParseError> {
    let mut identifier = parser.consume_plain_identifier()?;
    let mut prefix = Vec::new();
    while let Some(dot) = parser.skip_token(Token::Period) {
        prefix.push((identifier, dot));
        identifier = parser.consume_plain_identifier()?;
    }
    Ok(QualifiedName { prefix, identifier })
}
