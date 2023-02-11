use super::*;
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
        let mut output = format!("[{}]: {}\n", self.level, &self.title);
        for descr in &self.descriptions {
            if descr.verbosity > verbosity {
                // ignore those description with higher verbosity
                continue;
            }
            match &descr.span {
                Some(span) => {
                    output.push_str(&format!("{}:\n{}\n", span, descr.message,));
                }
                None => {
                    output.push_str(&descr.message);
                    output.push('\n');
                }
            }
        }
        output
    }

    pub fn report(&self, source: &str, verbosity: u8) -> String {
        let mut output = format!("[{}]: {}\n", self.level, &self.title);
        let text = source.lines().collect::<Vec<&str>>();
        for descr in &self.descriptions {
            if descr.verbosity > verbosity {
                // ignore those description with higher verbosity
                continue;
            }
            match &descr.span {
                Some(span) => {
                    let row_range = std::ops::Range {
                        start: span.start.row,
                        end: span.end.row + 1,
                    };

                    let mut vec: Vec<(usize, usize)> = Vec::new();
                    if span.start.row == span.end.row {
                        vec.push((span.start.col, span.end.col))
                    } else {
                        for row in row_range.clone() {
                            if row == span.start.row {
                                vec.push((span.start.col, text[row].len()))
                            } else if row == span.end.row {
                                vec.push((0, span.end.col))
                            } else {
                                vec.push((0, text[row].len()))
                            }
                        }
                    }

                    //println!("range = {:?}",range);
                    let head_width = (1 + span.end.row).to_string().len();

                    let zipped = row_range.zip(vec.into_iter());

                    for (row, (s, e)) in zipped {
                        // print header "xxx | ", where xxx is the line number
                        output.push_str(&format!("{:>.*} | {}\n", head_width, row + 1, text[row]));

                        output.push_str(&format!("{:>.*} | ", head_width, ' '));

                        for _ in 0..s {
                            output.push(' ');
                        }

                        if row == span.start.row {
                            output.push('^');
                            for _ in s + 1..e {
                                output.push('~');
                            }
                        } else if row == span.end.row {
                            for _ in s..e - 1 {
                                output.push('~');
                            }
                            output.push('^');
                        } else {
                            for _ in s..e {
                                output.push('~');
                            }
                        }
                        output.push('\n');
                    }
                    output.push_str(&descr.message);
                    output.push('\n');
                }
                None => {
                    output.push_str(&descr.message);
                    output.push('\n');
                }
            }
        }
        output
    }
}

#[test]
fn diagnostic_test() {
    let source = r#"1234567890
1234567890
1234567890
"#;

    let span = Span::new(Position::new(0, 5, 6), Position::new(2, 3, 25));
    let diag = Diagnostic::error("Error Name")
        .line("some error discreption")
        .line_span(span, "something spanned error discreption")
        .line_verb(100, "this should not appear!");

    assert_eq!(
        diag.minimal_report(30),
        r#"[Error]: Error Name
some error discreption
from line 1, col 6 to line 3, col 4:
something spanned error discreption
"#
    );
    assert_eq!(
        diag.report(source, 30),
        r#"[Error]: Error Name
some error discreption
1 | 1234567890
  |      ^~~~~
2 | 1234567890
  | ~~~~~~~~~~
3 | 1234567890
  | ~~^
something spanned error discreption
"#
    );
}
