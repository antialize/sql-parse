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
mod insert;
mod issue;
mod keywords;
mod lexer;
mod parser;
mod select;
mod span;
mod statement;
mod update;

pub use data_type::{DataType, DataTypeProperty, Type};
pub use issue::{Issue, Level};
pub use lexer::Lexer;
pub use span::{Span, Spanned};
pub use statement::Statement;

pub fn parse_statements(src: &str) -> (Vec<Statement<'_>>, Vec<Issue>) {
    let mut parser = Parser::new(src);
    let statements = statement::parse_statements(&mut parser);
    (statements, parser.issues)
}

// pub fn parse_statement(src: &str) -> (Option<Statement<'_>>, Vec<Issue>) {
//     let mut parser = Parser::new(src);
//     let statements = statement::parse_statement(&mut parser);
//     (statements.ok(), parser.issues)
// }

#[cfg(test)]
mod tests {

    use codespan_reporting::{
        diagnostic::{Diagnostic, Label},
        files::SimpleFiles,
        term::{
            self,
            termcolor::{ColorChoice, StandardStream},
        },
    };

    use crate::{issue::Level, parser::Parser, statement::parse_statements};

    #[test]
    fn it_works() {
        let src = std::fs::read_to_string("qs2.sql").expect("Failed to read file");
        //let src = "int (22) null signed unsigned signed,";
        let mut parser = Parser::new(&src);

        let statements = parse_statements(&mut parser);

        //let dt = parse_data_type(&mut parser);
        println!("{:#?}", parser.issues.len());

        if !parser.issues.is_empty() {
            let mut files = SimpleFiles::new();
            let file_id = files.add("qs2.sql", &src);
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();
            for issue in parser.issues {
                let mut labels = vec![Label::primary(file_id, issue.span)];
                for (message, span) in issue.fragments {
                    labels.push(Label::secondary(file_id, span).with_message(message));
                }
                let d = match issue.level {
                    Level::Error => Diagnostic::error(),
                    Level::Warning => Diagnostic::warning(),
                };
                let d = d.with_message(issue.message).with_labels(labels);
                term::emit(&mut writer.lock(), &config, &files, &d).unwrap();
            }
            panic!("HERE");
        }
        panic!();
    }
}
