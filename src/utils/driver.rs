use std::error::Error;
use std::fmt::Display;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process;

use crate::backend;
use crate::frontend;
use crate::frontend::diagnostic::ToDiagnostic;

#[derive(Debug)]
pub enum TopError {
    ParseError(frontend::parser::ParseError),
    RenameError(Vec<frontend::renamer::RenameError>),
    InferError(Vec<frontend::infer::InferError>),
}

impl Display for TopError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TopError::ParseError(err) => {
                write!(f, "Error: an error occured during parser phase")?;
                write!(f, "Cause: {err:?}")?;
            }
            TopError::RenameError(errs) => {
                write!(f, "Error: an error occured during rename phase")?;
                for err in errs {
                    write!(f, "Cause: {err:?}")?;
                }
            }
            TopError::InferError(errs) => {
                write!(f, "Error: an error occured during type-check phase")?;
                for err in errs {
                    write!(f, "Cause: {err:?}")?;
                }
            }
        }
        Ok(())
    }
}

impl Error for TopError {}

impl From<frontend::parser::ParseError> for TopError {
    fn from(value: frontend::parser::ParseError) -> Self {
        TopError::ParseError(value)
    }
}

impl From<Vec<frontend::renamer::RenameError>> for TopError {
    fn from(value: Vec<frontend::renamer::RenameError>) -> Self {
        TopError::RenameError(value)
    }
}

impl From<Vec<frontend::infer::InferError>> for TopError {
    fn from(value: Vec<frontend::infer::InferError>) -> Self {
        TopError::InferError(value)
    }
}

pub fn compile_source(source: &String, dump: bool) -> Result<String, TopError> {
    let mut par = frontend::parser::Parser::new(source);
    let mut expr = frontend::parser::parse_expr(&mut par)?;
    frontend::renamer::Renamer::run(&mut expr)?;
    frontend::infer::Infer::run(&expr)?;

    let expr = backend::normalize::Normalize::run(&expr);
    if dump {
        println!("normalize:\n{expr}");
    }
    let expr = backend::dead_elim::run_dead_elim(expr);
    if dump {
        println!("dead-elim:\n{expr}");
    }
    let expr = backend::const_fold::run_const_fold(expr);
    if dump {
        println!("const-fold:\n{expr}");
    }
    let expr = backend::linear_inline::run_linear_inline(expr);
    if dump {
        println!("linear-inline:\n{expr}");
    }
    let expr = backend::clos_conv::ClosConv::run(expr);
    if dump {
        println!("clos-conv:\n{expr}");
    }
    let expr = backend::dead_elim::run_dead_elim(expr);
    if dump {
        println!("dead-elim:\n{expr}");
    }
    let expr = backend::const_fold::run_const_fold(expr);
    if dump {
        println!("const-fold:\n{expr}");
    }
    let expr = backend::linear_inline::run_linear_inline(expr);
    if dump {
        println!("linear-inline:\n{expr}");
    }
    let text = backend::codegen::Codegen::run(&expr);
    if dump {
        println!("codegen:\n{text}");
    }
    Ok(text)
}

pub fn run_compile(input: &PathBuf, output: &PathBuf, dump: bool) -> Result<(), std::io::Error> {
    let source = fs::read_to_string(input)?;
    let result = compile_source(&source, dump);
    match result {
        Ok(result) => {
            let mut target = fs::File::create(output)?;
            target.write(result.as_bytes())?;
            Ok(())
        }
        Err(err) => {
            match err {
                TopError::ParseError(err) => {
                    let diag = err.to_diagnostic();
                    println!("{}", diag.report(&source, 30));
                }
                TopError::RenameError(errs) => {
                    for err in errs {
                        let diag = err.to_diagnostic();
                        println!("{}", diag.report(&source, 30));
                    }
                }
                TopError::InferError(errs) => {
                    for err in errs {
                        let diag = err.to_diagnostic();
                        println!("{}", diag.report(&source, 30));
                    }
                }
            }
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "compilation failed!",
            ))
        }
    }
}

pub fn run_link(code: &PathBuf, library: &PathBuf, output: &PathBuf) -> Result<(), std::io::Error> {
    if cfg!(target_os = "windows") {
        println!(
            "[WARNING] You are running one a windows os.\
Please make sure a C compiler is installed and 'cc' command is avaliable."
        );
    }
    let out = process::Command::new("cc")
        .arg(&code)
        .arg(&library)
        .arg("-o")
        .arg(output)
        .output()?;

    if out.status.success() {
        Ok(())
    } else {
        std::io::stdout().write(&out.stderr).unwrap();
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "link failed",
        ))
    }
}

pub fn run_compile_link(
    input: &PathBuf,
    library: &PathBuf,
    output: &PathBuf,
    dump: bool,
) -> Result<(), std::io::Error> {
    let temp = PathBuf::from("output.temp.c");
    run_compile(input, &temp, dump)?;
    run_link(&temp, library, output)?;
    fs::remove_file(temp)?;
    Ok(())
}
