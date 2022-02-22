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

use alloc::borrow::Cow;

use crate::{Span, Spanned};

/// A string with attached span
#[derive(Clone, Debug)]
pub struct SString<'a> {
    /// The underlying string
    pub value: Cow<'a, str>,
    /// The span the string originated from
    pub span: Span,
}

impl<'a> SString<'a> {
    /// Construct new SString with given value an span
    pub fn new(value: Cow<'a, str>, span: Span) -> Self {
        Self { value, span }
    }

    /// Return the str value
    pub fn as_str(&self) -> &str {
        self.value.as_ref()
    }
}

impl<'a> core::ops::Deref for SString<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.value.deref()
    }
}

impl<'a> Spanned for SString<'a> {
    fn span(&self) -> Span {
        self.span.span()
    }
}
