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

use core::fmt::Display;

use alloc::{borrow::Cow, vec::Vec};

use crate::{Span, Spanned};

/// Level of an issues
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Level {
    Warning,
    Error,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Fragment<'a> {
    /// The primary message of the fragmenst
    pub message: Cow<'static, str>,
    /// The span to attach the primary message to
    pub span: Span,
    /// The sql segment of the issue
    pub sql_segment: &'a str,
}

/// An issue encountered during parsing, or later stages
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Issue<'a> {
    /// The level of the issue
    pub level: Level,
    /// The primary message of the issue
    pub message: Cow<'static, str>,
    /// The span to attach the primary message to
    pub span: Span,
    /// The sql segment of the issue
    pub sql_segment: &'a str,
    /// List of secondary messages , spans and sql segments
    pub fragments: Vec<Fragment<'a>>,
}
pub struct IssueHandle<'a, 'b> {
    src: &'a str,
    issue: &'b mut Issue<'a>,
}

impl<'a, 'b> IssueHandle<'a, 'b> {
    pub fn frag(
        &mut self,
        message: impl Into<Cow<'static, str>>,
        span: &impl Spanned,
    ) -> &mut Self {
        let span: core::ops::Range<usize> = span.span();
        self.issue.fragments.push(Fragment {
            message: message.into(),
            span: span.clone(),
            sql_segment: &self.src[span.start..span.end],
        });
        self
    }
}


#[derive(Debug)]
pub struct Issues<'a> {
    pub src: &'a str,
    pub issues: Vec<Issue<'a>>,
}

impl<'a> Issues<'a> {
    pub fn err<'b>(
        &'b mut self,
        message: impl Into<Cow<'static, str>>,
        span: &impl Spanned,
    ) -> IssueHandle<'a, 'b> {
        let span: core::ops::Range<usize> = span.span();
        self.issues.push(Issue {
            level: Level::Error,
            message: message.into(),
            span: span.clone(),
            sql_segment: self.segment(span),
            fragments: Default::default(),
        });
        IssueHandle {
            src: self.src,
            issue: self.issues.last_mut().unwrap(),
        }
    }

    pub fn warn<'b>(
        &'b mut self,
        message: impl Into<Cow<'static, str>>,
        span: &impl Spanned,
    ) -> IssueHandle<'a, 'b> {
        let span = span.span();
        self.issues.push(Issue {
            level: Level::Warning,
            message: message.into(),
            span: span.clone(),
            sql_segment: self.segment(span),
            fragments: Default::default(),
        });
        IssueHandle {
            src: self.src,
            issue: self.issues.last_mut().unwrap(),
        }
    }

    pub fn segment(&self, span: Span) -> &'a str {
        &self.src[span.start..span.end]
    }

    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            issues: Default::default(),
        }
    }

    pub fn get(&self) -> &'_ [Issue<'a>] {
        &self.issues
    }

    pub fn into_vec(self) -> Vec<Issue<'a>> {
        self.issues
    }

    pub fn is_ok(&self) -> bool {
        self.issues.is_empty()
    }
}

impl<'a> Display for Issues<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.issues.is_empty() {
            return writeln!(f, "No issues");
        }
        writeln!(f, "Issues:")?;
        for issue in self.get() {
            match issue.level {
                Level::Warning => write!(f, "  Warning ")?,
                Level::Error => write!(f, "  Error ")?,
            }
            writeln!(f, "{}", issue.message)?;
            let line = self.src[..issue.span.start]
                .chars()
                .filter(|v| *v == '\n')
                .count()
                + 1;
            writeln!(f, "    At line {}, code {}", line, issue.sql_segment)?;
            for fragment in &issue.fragments {
                writeln!(f, "  {}", fragment.message)?;
                let line = self.src[..fragment.span.start]
                    .chars()
                    .filter(|v| *v == '\n')
                    .count()
                    + 1;
                writeln!(f, "    At line {}, code {}", line, fragment.sql_segment)?;
            }
        }
        Ok(())
    }
}
