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

/// Byte span of ast fragment
pub type Span = core::ops::Range<usize>;

pub trait OptSpanned {
    fn opt_span(&self) -> Option<Span>;

    fn opt_join_span(&self, other: &impl OptSpanned) -> Option<Span> {
        if let Some(l) = self.opt_span() {
            Some(l.join_span(other))
        } else {
            other.opt_span()
        }
    }
}

/// Return or compute byte span of ast fragment
pub trait Spanned {
    /// Return or compute byte span of ast fragment
    fn span(&self) -> Span;

    fn join_span(&self, other: &impl OptSpanned) -> Span {
        let l = self.span();
        if let Some(r) = other.opt_span() {
            usize::min(l.start, r.start)..usize::max(l.end, r.end)
        } else {
            l
        }
    }
}

impl<T: Spanned> OptSpanned for T {
    fn opt_span(&self) -> Option<Span> {
        Some(self.span())
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        self.clone()
    }
}

impl<T: Spanned> Spanned for Box<T> {
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

impl<T: OptSpanned> OptSpanned for Option<T> {
    fn opt_span(&self) -> Option<Span> {
        match &self {
            Some(v) => v.opt_span(),
            None => None,
        }
    }
}

impl<T: OptSpanned> OptSpanned for Vec<T> {
    fn opt_span(&self) -> Option<Span> {
        self.iter().fold(None, |a, b| a.opt_join_span(b))
    }
}

impl<S: Spanned> Spanned for (usize, S) {
    fn span(&self) -> Span {
        self.1.span()
    }
}

impl<S: Spanned> Spanned for (usize, usize, S) {
    fn span(&self) -> Span {
        self.2.span()
    }
}

impl<S: Spanned> Spanned for (u32, S) {
    fn span(&self) -> Span {
        self.1.span()
    }
}

impl<S: Spanned> Spanned for (u64, S) {
    fn span(&self) -> Span {
        self.1.span()
    }
}

impl<S: Spanned> Spanned for (f64, S) {
    fn span(&self) -> Span {
        self.1.span()
    }
}

impl<S: Spanned> Spanned for (bool, S) {
    fn span(&self) -> Span {
        self.1.span()
    }
}

impl<'a, S: Spanned> Spanned for (&'a str, S) {
    fn span(&self) -> Span {
        self.1.span()
    }
}

impl<'a, S: Spanned> Spanned for (std::borrow::Cow<'a, str>, S) {
    fn span(&self) -> Span {
        self.1.span()
    }
}

impl<S: Spanned, O: OptSpanned> Spanned for (S, O) {
    fn span(&self) -> Span {
        self.0.join_span(&self.1)
    }
}

impl<T1: Spanned, T2: OptSpanned, T3: OptSpanned> Spanned for (T1, T2, T3) {
    fn span(&self) -> Span {
        self.0.join_span(&self.1).join_span(&self.2)
    }
}
