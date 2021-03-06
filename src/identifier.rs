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

use crate::{Span, Spanned};

/// Simple identifier in code
/// it derefs to its string value
#[derive(Clone, Debug)]
pub struct Identifier<'a> {
    /// Identifier string
    pub value: &'a str,
    /// Span of the value
    pub span: Span,
}

impl<'a> Identifier<'a> {
    /// Produce new identifier given value and span
    pub fn new(value: &'a str, span: Span) -> Self {
        Identifier { value, span }
    }

    /// Get the string representation of the identifier
    pub fn as_str(&self) -> &'a str {
        self.value
    }
}

impl<'a> core::ops::Deref for Identifier<'a> {
    type Target = str;

    fn deref(&self) -> &'a Self::Target {
        self.value
    }
}

impl<'a> Spanned for Identifier<'a> {
    fn span(&self) -> Span {
        self.span.span()
    }
}
