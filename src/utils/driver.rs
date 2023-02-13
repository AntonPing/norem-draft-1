use std::error::Error;
use std::fmt::Display;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process;

use crate::backend;
use crate::frontend;

#[derive(Debug)]
pub enum TopError {
    ParseError(crate::frontend::parser::ParseError),
    IOError(std::io::Error),
}

impl Display for TopError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TopError::ParseError(err) => {
                write!(f, "Error: an error occured during parser phase")?;
                write!(f, "Cause: {err:?}")?;
            }
            TopError::IOError(err) => {
                write!(f, "Error: an IO error occured!")?;
                write!(f, "Cause: {err:?}")?;
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

impl From<std::io::Error> for TopError {
    fn from(value: std::io::Error) -> Self {
        TopError::IOError(value)
    }
}

pub fn compile_source(source: String, dump: bool) -> Result<String, TopError> {
    let mut par = frontend::parser::Parser::new(&source);
    let expr = frontend::parser::parse_expr(&mut par)?;
    let mut rnm = frontend::renamer::Renamer::new();
    let expr = rnm.visit_expr(expr);
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

pub fn run_compile(input: &PathBuf, output: &PathBuf, dump: bool) -> Result<(), TopError> {
    let source = fs::read_to_string(input)?;
    let result = compile_source(source, dump)?;
    let mut target = fs::File::create(output)?;
    target.write(result.as_bytes())?;
    Ok(())
}

pub fn run_link(code: &PathBuf, library: &PathBuf, output: &PathBuf) -> Result<(), TopError> {
    if cfg!(target_os = "windows") {
        println!(
            "[WARNING] You are running one a windows os.\
Please make sure a C compiler is installed and 'cc' command is avaliable."
        );
    }
    process::Command::new("cc")
        .arg(&code)
        .arg(&library)
        .arg("-o")
        .arg(output)
        .output()?;
    Ok(())
}

pub fn run_compile_link(
    input: &PathBuf,
    library: &PathBuf,
    output: &PathBuf,
    dump: bool,
) -> Result<(), TopError> {
    let temp = PathBuf::from("output.temp.c");
    run_compile(input, &temp, dump)?;
    run_link(&temp, library, output)?;
    fs::remove_file(temp)?;
    Ok(())
}
