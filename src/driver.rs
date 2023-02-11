use std::error::Error;
use std::fmt::Display;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process;

use crate::clos_conv::ClosConv;
use crate::codegen::Codegen;
use crate::normalize::Normalize;
use crate::parser::{parse_expr, Parser};
use crate::renamer::Renamer;
use crate::simple_opt::{ConstFold, DeadElim, LinearInline};

#[derive(Debug)]
pub enum TopError {
    ParseError(crate::parser::ParseError),
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

impl From<crate::parser::ParseError> for TopError {
    fn from(value: crate::parser::ParseError) -> Self {
        TopError::ParseError(value)
    }
}

impl From<std::io::Error> for TopError {
    fn from(value: std::io::Error) -> Self {
        TopError::IOError(value)
    }
}

pub fn compile_source(source: String, dump: bool) -> Result<String, TopError> {
    let mut par = Parser::new(&source);
    let expr = parse_expr(&mut par)?;

    let mut rnm = Renamer::new();
    let expr = rnm.visit_expr(expr);
    let expr = Normalize::run(&expr);
    if dump {
        println!("normalize:\n{expr}");
    }
    let expr = DeadElim::run(expr);
    if dump {
        println!("dead-elim:\n{expr}");
    }
    let expr = ConstFold::run(expr);
    if dump {
        println!("const-fold:\n{expr}");
    }
    let expr = LinearInline::run(expr);
    if dump {
        println!("linear-inline:\n{expr}");
    }
    let expr = ClosConv::run(expr);
    if dump {
        println!("clos-conv:\n{expr}");
    }
    let expr = DeadElim::run(expr);
    if dump {
        println!("dead-elim:\n{expr}");
    }
    let expr = ConstFold::run(expr);
    if dump {
        println!("const-fold:\n{expr}");
    }
    let expr = LinearInline::run(expr);
    if dump {
        println!("linear-inline:\n{expr}");
    }
    let text = Codegen::run(&expr);
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
