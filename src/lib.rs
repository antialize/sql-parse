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

//! Parse SQL into an AST
//!
//! This crate provides an lexer and parser that can parse SQL
//! into an Abstract Syntax Tree (AST). Currently primarily focused
//! on MariaDB/Mysql.
//!
//! Example code:
//!
//! ```
//! use sql_parse::{SQLDialect, SQLArguments, ParseOptions, parse_statement, Issues};
//!
//! let options = ParseOptions::new()
//!     .dialect(SQLDialect::MariaDB)
//!     .arguments(SQLArguments::QuestionMark)
//!     .warn_unquoted_identifiers(true);
//!
//!
//! let sql = "SELECT `monkey`,
//!            FROM `t1` LEFT JOIN `t2` ON `t2`.`id` = `t1.two`
//!            WHERE `t1`.`id` = ?";
//! let mut issues = Issues::new(sql);
//! let ast = parse_statement(sql, &mut issues, &options);
//!
//! println!("{}", issues);
//! println!("AST: {:#?}", ast);
//! ```
//!

#![no_std]
#![forbid(unsafe_code)]
extern crate alloc;

use alloc::vec::Vec;
use lexer::Token;
use parser::Parser;
mod alter;
mod create;
mod data_type;
mod delete;
mod drop;
mod expression;
mod identifier;
mod insert_replace;
mod issue;
mod keywords;
mod lexer;
mod parser;
mod qualified_name;
mod rename;
mod select;
mod span;
mod sstring;
mod statement;
mod truncate;
mod update;
mod with_query;

pub use data_type::{DataType, DataTypeProperty, Type};
pub use identifier::Identifier;
pub use issue::{Fragment, Issue, IssueHandle, Issues, Level};
pub use qualified_name::QualifiedName;
pub use span::{OptSpanned, Span, Spanned};
pub use sstring::SString;
pub use statement::{Statement, Union, UnionType, UnionWith};

pub use alter::{
    AlterColumnAction, AlterSpecification, AlterTable, ForeignKeyOn, ForeignKeyOnAction,
    ForeignKeyOnType, IndexCol, IndexOption, IndexType,
};
pub use create::{
    CreateAlgorithm, CreateDefinition, CreateFunction, CreateOption, CreateTable, CreateTrigger,
    CreateView, TableOption,
};
pub use delete::{Delete, DeleteFlag};
pub use drop::{
    DropDatabase, DropEvent, DropFunction, DropIndex, DropProcedure, DropServer, DropTable,
    DropTrigger, DropView,
};
pub use expression::{
    BinaryOperator, Expression, Function, IdentifierPart, Is, UnaryOperator, Variable, When,
};
pub use insert_replace::{
    InsertReplace, InsertReplaceFlag, InsertReplaceOnDuplicateKeyUpdate, InsertReplaceSet,
    InsertReplaceSetPair, InsertReplaceType, OnConflict, OnConflictAction, OnConflictTarget,
};
pub use rename::{RenameTable, TableToTable};
pub use select::{
    IndexHint, IndexHintFor, IndexHintType, IndexHintUse, JoinSpecification, JoinType, Select,
    SelectExpr, SelectFlag, TableReference,
};
pub use truncate::TruncateTable;
pub use update::{Update, UpdateFlag};
pub use with_query::{WithBlock, WithQuery};

/// What sql diarect to parse as
#[derive(Clone, Debug)]
pub enum SQLDialect {
    /// Parse MariaDB/Mysql SQL
    MariaDB,
    PostgreSQL,
    Sqlite,
}

impl SQLDialect {
    pub fn is_postgresql(&self) -> bool {
        matches!(self, SQLDialect::PostgreSQL)
    }

    pub fn is_maria(&self) -> bool {
        matches!(self, SQLDialect::MariaDB)
    }

    pub fn is_sqlite(&self) -> bool {
        matches!(self, SQLDialect::Sqlite)
    }
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
    /// Arguments ar #i
    Dollar,
}

/// Options used when parsing sql
#[derive(Clone, Debug)]
pub struct ParseOptions {
    dialect: SQLDialect,
    arguments: SQLArguments,
    warn_unquoted_identifiers: bool,
    warn_none_capital_keywords: bool,
    list_hack: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            dialect: SQLDialect::MariaDB,
            arguments: SQLArguments::None,
            warn_none_capital_keywords: false,
            warn_unquoted_identifiers: false,
            list_hack: false,
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

    pub fn get_dialect(&self) -> SQLDialect {
        self.dialect.clone()
    }

    /// Change what kinds of arguments are supplied
    pub fn arguments(self, arguments: SQLArguments) -> Self {
        Self { arguments, ..self }
    }

    /// Should we warn about unquoted identifiers
    pub fn warn_unquoted_identifiers(self, warn_unquoted_identifiers: bool) -> Self {
        Self {
            warn_unquoted_identifiers,
            ..self
        }
    }

    /// Should we warn about unquoted identifiers
    pub fn warn_none_capital_keywords(self, warn_none_capital_keywords: bool) -> Self {
        Self {
            warn_none_capital_keywords,
            ..self
        }
    }

    /// Parse _LIST_ as special expression
    pub fn list_hack(self, list_hack: bool) -> Self {
        Self { list_hack, ..self }
    }
}

/// Construct an "Internal compiler error" issue, containing the current file and line
#[macro_export]
macro_rules! issue_ice {
    ( $issues: expr, $spanned:expr ) => {{
        $issues.err(
            alloc::format!("Internal compiler error in {}:{}", file!(), line!()),
            $spanned,
        );
    }};
}

/// Construct an "Not yet implemented" issue, containing the current file and line
#[macro_export]
macro_rules! issue_todo {
    ( $issues: expr, $spanned:expr ) => {{
        $issues.err(
            alloc::format!("Not yet implemented {}:{}", file!(), line!()),
            $spanned,
        );
    }};
}

/// Parse multiple statements,
/// return an Vec of Statements even if there are parse errors.
/// The statements are free of errors if no Error issues are
/// added to issues
pub fn parse_statements<'a>(
    src: &'a str,
    issues: &mut Issues<'a>,
    options: &ParseOptions,
) -> Vec<Statement<'a>> {
    let mut parser = Parser::new(src, issues, options);
    statement::parse_statements(&mut parser)
}

/// Parse a single statement,
/// A statement may be returned even if there where parse errors.
/// The statement is free of errors if no Error issues are
/// added to issues
pub fn parse_statement<'a>(
    src: &'a str,
    issues: &mut Issues<'a>,
    options: &ParseOptions,
) -> Option<Statement<'a>> {
    let mut parser = Parser::new(src, issues, options);
    match statement::parse_statement(&mut parser) {
        Ok(Some(v)) => {
            if parser.token != Token::Eof {
                parser.expected_error("Unexpected token after statement")
            }
            Some(v)
        }
        Ok(None) => {
            parser.expected_error("Statement");
            None
        }
        Err(_) => None,
    }
}

#[test]
pub fn test_parse_alter_sql() {
    let sql = "ALTER TABLE `test` ADD COLUMN `test1` VARCHAR (128) NULL DEFAULT NULL";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    parse_statement(sql, &mut issues, &options);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn test_parse_delete_sql_with_schema() {
    let sql = "DROP TABLE IF EXISTS `test_schema`.`test`";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    parse_statement(sql, &mut issues, &options);
    assert!(issues.is_ok(), "{}", issues);
}
#[test]
pub fn parse_create_index_sql_with_schema() {
    let sql = "CREATE INDEX `idx_test` ON  test_schema.test(`col_test`)";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    parse_statement(sql, &mut issues, &options);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn parse_drop_index_sql_with_schema() {
    let sql = "DROP INDEX `idx_test` ON  test_schema.test";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    let _result = parse_statement(sql, &mut issues, &options);
    // assert!(result.is_none(), "result: {:#?}", &result);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn parse_create_view_sql_with_schema() {
    let sql =
        "CREATE OR REPLACE VIEW `test_schema`.`view_test` AS SELECT * FROM `test_schema`.`test`";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    let _result = parse_statement(sql, &mut issues, &options);
    // assert!(result.is_none(), "result: {:#?}", &result);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn parse_drop_view_sql_with_schema() {
    let sql = "DROP VIEW `test_schema`.`view_test`";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    let _result = parse_statement(sql, &mut issues, &options);
    // assert!(result.is_none(), "result: {:#?}", &result);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn parse_truncate_table_sql_with_schema() {
    let sql = "TRUNCATE TABLE `test_schema`.`table_test`";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    let _result = parse_statement(sql, &mut issues, &options);
    // assert!(result.is_none(), "result: {:#?}", &result);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn parse_rename_table_sql_with_schema() {
    let sql = "RENAME TABLE `test_schema`.`table_test` To `test_schema`.`table_new_test`";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    let _result = parse_statement(sql, &mut issues, &options);
    // assert!(result.is_none(), "result: {:#?}", &result);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn parse_with_statement() {
    let sql = "
        WITH monkeys AS (DELETE FROM thing RETURNING id),
        baz AS (SELECT id FROM cats WHERE comp IN (monkeys))
        DELETE FROM dogs WHERE cat IN (cats)";
    let options = ParseOptions::new()
        .dialect(SQLDialect::PostgreSQL)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    let _result = parse_statement(sql, &mut issues, &options);
    // assert!(result.is_none(), "result: {:#?}", &result);
    assert!(issues.is_ok(), "{}", issues);
}

#[test]
pub fn parse_use_index() {
    let sql = "SELECT `a` FROM `b` FORCE INDEX FOR GROUP BY (`b`, `c`)";
    let options = ParseOptions::new()
        .dialect(SQLDialect::MariaDB)
        .arguments(SQLArguments::QuestionMark)
        .warn_unquoted_identifiers(false);

    let mut issues = Issues::new(sql);
    let _result = parse_statement(sql, &mut issues, &options);
    assert!(issues.is_ok(), "{}", issues);
}
