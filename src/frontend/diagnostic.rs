use itertools::Itertools;

use super::{infer::InferError, *};
use std::fmt;

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum DiagLevel {
    Error,
    Warn,
    Info,
}

impl fmt::Display for DiagLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DiagLevel::Error => write!(f, "Error"),
            DiagLevel::Warn => write!(f, "Warn"),
            DiagLevel::Info => write!(f, "Info"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Description {
    verbosity: u8,
    message: String,
    span: Option<Span>,
}

impl Description {
    pub fn message<S: Into<String>>(msg: S) -> Description {
        Description {
            verbosity: 10,
            message: msg.into(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Description {
        self.span = Some(span);
        self
    }

    pub fn with_verbosity(mut self, verbosity: u8) -> Description {
        self.verbosity = verbosity;
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Diagnostic {
    level: DiagLevel,
    title: String,
    descriptions: Vec<Description>,
}

impl Diagnostic {
    pub fn error<S: Into<String>>(title: S) -> Diagnostic {
        Diagnostic {
            level: DiagLevel::Error,
            title: title.into(),
            descriptions: Vec::new(),
        }
    }

    pub fn warn<S: Into<String>>(title: S) -> Diagnostic {
        Diagnostic {
            level: DiagLevel::Warn,
            title: title.into(),
            descriptions: Vec::new(),
        }
    }

    pub fn info<S: Into<String>>(title: S) -> Diagnostic {
        Diagnostic {
            level: DiagLevel::Info,
            title: title.into(),
            descriptions: Vec::new(),
        }
    }

    pub fn line<S: Into<String>>(mut self, msg: S) -> Diagnostic {
        self.descriptions.push(Description::message(msg));
        self
    }

    pub fn line_span<S: Into<String>>(mut self, span: Span, msg: S) -> Diagnostic {
        self.descriptions
            .push(Description::message(msg).with_span(span));
        self
    }

    pub fn line_verb<S: Into<String>>(mut self, verbosity: u8, msg: S) -> Diagnostic {
        self.descriptions
            .push(Description::message(msg).with_verbosity(verbosity));
        self
    }

    pub fn line_span_verb<S: Into<String>>(
        mut self,
        span: Span,
        verbosity: u8,
        msg: S,
    ) -> Diagnostic {
        self.descriptions.push(
            Description::message(msg)
                .with_span(span)
                .with_verbosity(verbosity),
        );
        self
    }

    /// minimal_report shows only span, instead of source code.
    pub fn minimal_report(&self, verbosity: u8) -> String {
        let mut output = format!("[{}] {}\n", self.level, &self.title);
        for descr in &self.descriptions {
            if descr.verbosity > verbosity {
                // ignore those description with higher verbosity
                continue;
            }
            output.push_str(&descr.message);
            output.push('\n');
            match &descr.span {
                Some(span) => {
                    output.push_str(&format!("{}\n", span));
                }
                None => {
                    // do nothing
                }
            }
        }
        output
    }

    pub fn report(&self, source: &str, verbosity: u8) -> String {
        let mut output = format!("[{}] {}\n", self.level, &self.title);
        let text = source.lines().collect::<Vec<&str>>();
        for descr in &self.descriptions {
            if descr.verbosity > verbosity {
                // ignore those description with higher verbosity
                continue;
            }
            output.push_str(&descr.message);
            output.push('\n');
            match &descr.span {
                // one-line span
                Some(span) if span.start.row == span.end.row => {
                    let row = span.start.row;
                    let head_width = (1 + row).to_string().len();
                    let spaces = span.start.col;
                    let waves = span.end.col - span.start.col - 2;
                    output.push_str(&format!("{:<head_width$} | {}\n", row + 1, text[row]));
                    output.push_str(&format!(
                        "{:<head_width$} | {:<spaces$}^{:~<waves$}^\n",
                        "", "", ""
                    ));
                }
                // more than one line
                Some(span) => {
                    let row_range = std::ops::Range {
                        start: span.start.row,
                        end: span.end.row + 1,
                    };
                    let head_width = (1 + span.end.row).to_string().len();
                    for row in row_range {
                        if row == span.start.row {
                            let spaces = span.start.col;
                            let waves = text[row].len() - spaces - 1;
                            output.push_str(&format!("{:<head_width$} | {}\n", row + 1, text[row]));
                            output.push_str(&format!(
                                "{:<head_width$} | {:<spaces$}^{:~<waves$}\n",
                                "", "", ""
                            ));
                        } else if row == span.end.row {
                            let waves = span.end.col;
                            output.push_str(&format!("{:<head_width$} | {}\n", row + 1, text[row]));
                            output.push_str(&format!("{:<head_width$} | {:~<waves$}^\n", "", ""));
                        } else {
                            output.push_str(&format!("{:<head_width$} | {}\n", row + 1, text[row]));
                        }
                    }
                }
                None => {
                    // do nothing
                }
            }
        }
        output
    }
}

pub trait ToDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic;
}

impl ToDiagnostic for parser::ParseError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            parser::ParseError::LexerError(span, msg) => Diagnostic::error("lexer error")
                .line(msg.clone())
                .line_span(span.clone(), "failed to read token here"),
            parser::ParseError::Unexpected(span, found, expect) => {
                Diagnostic::error("parser Error: unexpected Token")
                    .line(format!(
                        "found token {:?}, expected token {:?}",
                        found, expect
                    ))
                    .line_span(span.clone(), "unexpected token is here")
            }
            parser::ParseError::UnexpectedMany(span, found, expect) => {
                Diagnostic::error("parser error: unexpected Token")
                    .line(format!("found token {:?},", found))
                    .line(format!("expected one token of {:?}", expect))
                    .line_span(span.clone(), "unexpected token is here")
            }
            parser::ParseError::UnknownBuiltin(span, str) => {
                Diagnostic::error("parser error: unknown builtin primitive")
                    .line(format!("found unknown primitive {:?}", str))
                    .line_span(span.clone(), "unknown primitive is here")
            }
        }
    }
}

impl ToDiagnostic for renamer::RenameError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            renamer::RenameError::UnboundedValueVariable(span, ident) => {
                Diagnostic::error("scope error: unbounded value variable")
                    .line(format!("found unbounded variable {ident}"))
                    .line_span(span.clone(), "unbounded variable is here")
            }
            renamer::RenameError::UnboundedTypeVariable(span, ident) => {
                Diagnostic::error("scope error: unbounded type variable")
                    .line(format!("found unbounded variable {ident}"))
                    .line_span(span.clone(), "unbounded variable is here")
            }
            renamer::RenameError::UnboundedConstructorVariable(span, ident) => {
                Diagnostic::error("scope error: unbounded constructor variable")
                    .line(format!("found unbounded variable {ident}"))
                    .line_span(span.clone(), "unbounded variable is here")
            }
            renamer::RenameError::UndefinedExternalFunction(span, ident) => {
                Diagnostic::error("scope error: unbounded type variable")
                    .line(format!("found unbounded variable {ident}"))
                    .line_span(span.clone(), "unbounded variable is here")
            }
            renamer::RenameError::MultipuleDefinition(span, ident) => {
                Diagnostic::error("scope error: multiple definitions of same variable")
                    .line(format!("multiple definitions of {ident}"))
                    .line_span(span.clone(), "multiple definitions are here")
            }
            renamer::RenameError::MultipuleExternalDefinition(span, ident) => {
                Diagnostic::error("scope error: multiple external function definitions")
                    .line(format!("multiple definitions of {ident}"))
                    .line_span(span.clone(), "multiple definitions are here")
            }
        }
    }
}

impl ToDiagnostic for infer::InferError {
    fn to_diagnostic(&self) -> Diagnostic {
        let InferError { title, spans, errs } = self;
        let mut diag = Diagnostic::error(format!("type error: {title:?}"));
        for (msg, span) in spans {
            diag = diag.line_span(span.clone(), msg.clone());
        }
        for err in errs {
            match err {
                infer::UnifyError::CantUnify(lhs, rhs) => {
                    diag = diag.line(format!("can't unify \"{lhs}\" with \"{rhs}\""));
                }
                infer::UnifyError::CantUnifyVec(lhs, rhs) => {
                    let lhs = lhs.iter().format(",");
                    let rhs = rhs.iter().format(",");
                    diag = diag.line(format!("can't unify [{lhs}] with [{rhs}]"));
                }
                infer::UnifyError::OccurCheckFailed(x, ty) => {
                    diag = diag.line(format!("occur check fail {x} in \"{ty}\""));
                }
                infer::UnifyError::DiffConstr(lhs, rhs) => {
                    diag = diag.line(format!("can't unify \"{lhs}\" with \"{rhs}\""));
                }
            }
        }
        diag
    }
}

#[test]
fn diagnostic_test() {
    let source = r#"1234567890
1234567890
1234567890
"#;

    let span = Span::new(Position::new(0, 5, 5), Position::new(2, 5, 25));
    let diag = Diagnostic::error("Error Name")
        .line("some error discreption")
        .line_span(span, "some spanned error discreption")
        .line_verb(100, "this should not appear!");

    // println!("{}", diag.minimal_report(30));
    assert_eq!(
        diag.minimal_report(30),
        r#"[Error] Error Name
some error discreption
some spanned error discreption
from line 1, col 6 to line 3, col 6
"#
    );

    // println!("{}", diag.report(source, 30));
    assert_eq!(
        diag.report(source, 30),
        r#"[Error] Error Name
some error discreption
some spanned error discreption
1 | 1234567890
  |      ^~~~~
2 | 1234567890
3 | 1234567890
  | ~~~~~^
"#
    );
}
