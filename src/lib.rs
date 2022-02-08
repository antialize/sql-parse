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

use parser::Parser;
mod alter;
mod create;
mod data_type;
mod delete;
mod drop;
mod expression;
mod identifier;
mod insert;
mod issue;
mod keywords;
mod lexer;
mod parser;
mod replace;
mod select;
mod span;
mod sstring;
mod statement;
mod update;

pub use data_type::{DataType, DataTypeProperty, Type};
pub use identifier::Identifier;
pub use issue::{Issue, Level};
pub use lexer::Lexer;
pub use span::{OptSpanned, Span, Spanned};
pub use sstring::SString;
pub use statement::{Statement, Union, UnionType, UnionWith};

pub use alter::{AlterSpecification, AlterTable};
pub use create::{
    CreateAlgorithm, CreateDefinition, CreateFunction, CreateOption, CreateTable, CreateTrigger,
    CreateView, TableOption,
};
pub use delete::{Delete, DeleteFlag};
pub use drop::{
    DropDatabase, DropEvent, DropFunction, DropProcedure, DropServer, DropTable, DropTrigger,
    DropView,
};
pub use expression::{
    BinaryOperator, Expression, Function, IdentifierPart, Is, UnaryOperator, When,
};
pub use insert::{Insert, InsertFlag};
pub use replace::{Replace, ReplaceFlag};
pub use select::{JoinSpecification, JoinType, Select, SelectFlag, TableReference};
pub use update::{Update, UpdateFlag};

/// What sql diarect to parse as
#[derive(Clone, Debug)]
pub enum SQLDialect {
    /// Parse MariaDB/Mysql SQL
    MariaDB,
}

/// What kinds or arguments
#[derive(Clone, Debug)]
pub enum SQLArguments {
    /// The statements do not contain arguments
    None,
    /// Arguments are %s or %d
    Percent,
    /// Arguments are ?
    QuestionMark,
}

/// Options used when parsing xml
#[derive(Clone, Debug)]
pub struct ParseOptions {
    dialect: SQLDialect,
    arguments: SQLArguments,
    warn_unqouted_identifiers: bool,
    warn_none_capital_keywords: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            dialect: SQLDialect::MariaDB,
            arguments: SQLArguments::None,
            warn_none_capital_keywords: false,
            warn_unqouted_identifiers: false,
        }
    }
}

impl ParseOptions {
    pub fn new() -> Self {
        Default::default()
    }

    /// Change whan SQL dialect to use
    pub fn dialect(self, dialect: SQLDialect) -> Self {
        Self { dialect, ..self }
    }

    /// Change what kinds of arguments are supplied
    pub fn arguments(self, arguments: SQLArguments) -> Self {
        Self { arguments, ..self }
    }

    /// Should we warn about unqueted identifiers
    pub fn warn_unqouted_identifiers(self, warn_unqouted_identifiers: bool) -> Self {
        Self {
            warn_unqouted_identifiers,
            ..self
        }
    }

    /// Should we warn about unqueted identifiers
    pub fn warn_none_capital_keywords(self, warn_none_capital_keywords: bool) -> Self {
        Self {
            warn_none_capital_keywords,
            ..self
        }
    }
}

#[macro_export]
macro_rules! issue_ice {
    ( $spanned:expr ) => {
        {
            Issue::err(
                format!("Internal compiler error in {}:{}", file!(), line!()),
                $spanned
            )
        }
    };
}

#[macro_export]
macro_rules! issue_todo {
    ( $spanned:expr ) => {
        {
            Issue::err(
                format!("Not yet implemented {}:{}", file!(), line!()),
                $spanned
            )
        }
    };
}

/// Parse multiple statements,
/// return an vec of Statements even if there are parse errors.
pub fn parse_statements<'a>(
    src: &'a str,
    issues: &mut Vec<Issue>,
    options: &ParseOptions,
) -> Vec<Statement<'a>> {
    let mut parser = Parser::new(src, issues, options);
    statement::parse_statements(&mut parser)
}

/// Parse a single statement,
/// A statement may be returned even if there where parse errors.
pub fn parse_statement<'a>(
    src: &'a str,
    issues: &mut Vec<Issue>,
    options: &ParseOptions,
) -> Option<Statement<'a>> {
    let mut parser = Parser::new(src, issues, options);
    statement::parse_statement(&mut parser).unwrap_or_default()
}
