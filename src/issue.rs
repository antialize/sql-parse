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

use alloc::{string::String, vec::Vec};

use crate::{Span, Spanned};

/// Level of an issues
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Level {
    Warning,
    Error,
}

/// An issue encountered during parsing, or later stages
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Issue {
    /// The level of the issue
    pub level: Level,
    /// The primary message of the issue
    pub message: String,
    /// The span to attach the primary message to
    pub span: Span,
    /// List of secondary messages and spans
    pub fragments: Vec<(String, Span)>,
}

impl Issue {
    /// Construct an error with given message and span
    pub fn err(message: impl Into<String>, span: &impl Spanned) -> Self {
        Issue {
            level: Level::Error,
            message: message.into(),
            span: span.span(),
            fragments: Vec::new(),
        }
    }

    /// Construct a warning with given message and span
    pub fn warn(message: impl Into<String>, span: &impl Spanned) -> Self {
        Issue {
            level: Level::Warning,
            message: message.into(),
            span: span.span(),
            fragments: Vec::new(),
        }
    }

    /// Add a fragment with the given message and span
    pub fn frag(mut self, message: impl Into<String>, span: &impl Spanned) -> Self {
        self.fragments.push((message.into(), span.span()));
        self
    }
}
