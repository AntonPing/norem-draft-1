use norem::utils::driver;

fn main() {
    use std::path::PathBuf;
    extern crate clap;
    use clap::{Arg, ArgAction, Command};

    let matches = Command::new("norem")
        .version("0.1.0")
        .author("Anton Ping <antonping1999@gmail.com>")
        .about(
            "\
            A native-compiled functional programming language.\
            The word \"norem\" stands for \"not really monadic\".\
        ",
        )
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("compile")
                .about("compile norem source file to target language")
                .arg(
                    Arg::new("INPUT")
                        .required(true)
                        .help("path of input norem source file"),
                )
                .arg(
                    Arg::new("OUTPUT")
                        .short('o')
                        .long("output")
                        .required(false)
                        .help("path for saving output file"),
                )
                .arg(
                    Arg::new("DUMP")
                        .short('d')
                        .long("dump")
                        .required(false)
                        .action(ArgAction::SetTrue)
                        .help("print intermediate result of compiliation"),
                ),
        )
        .subcommand(
            Command::new("link")
                .about("link compiled norem file with external library")
                .arg(
                    Arg::new("CODE")
                        .required(true)
                        .help("path of compiled norem file"),
                )
                .arg(
                    Arg::new("LIBRARY")
                        .required(true)
                        .help("path of external library"),
                )
                .arg(
                    Arg::new("OUTPUT")
                        .short('o')
                        .long("output")
                        .required(false)
                        .help("path for saving output file"),
                ),
        )
        .get_matches();

    match matches.subcommand().unwrap() {
        ("compile", sub_matches) => {
            let input: PathBuf = sub_matches
                .get_one::<String>("INPUT")
                .map(|x| x.into())
                .unwrap();

            if !matches!(input.extension(), Some(x) if x == "nrm") {
                panic!("norem source name file should end with '.nrm'!");
            }

            let output: PathBuf = sub_matches
                .get_one::<String>("OUTPUT")
                .map(|x| x.into())
                .unwrap_or(PathBuf::from("output.c"));

            if !matches!(output.extension(), Some(x) if x == "c") {
                panic!("output file name should end with '.c'!");
            }

            let dump = sub_matches.get_flag("DUMP");
            match driver::run_compile(&input, &output, dump) {
                Ok(()) => {
                    println!("compilation successed.");
                }
                Err(err) => {
                    println!("{err}");
                    println!("compilation failed!");
                }
            }
        }
        ("link", sub_matches) => {
            let code: PathBuf = sub_matches
                .get_one::<String>("CODE")
                .map(|x| x.into())
                .unwrap();

            if !matches!(code.extension(), Some(x) if x == "c") {
                panic!("compiled code file name should end with '.c'!");
            }

            let library: PathBuf = sub_matches
                .get_one::<String>("LIBRARY")
                .map(|x| x.into())
                .unwrap();

            if !matches!(library.extension(), Some(x) if x == "c") {
                panic!("external library file name should end with '.c'!");
            }

            let output: PathBuf = sub_matches
                .get_one::<String>("OUTPUT")
                .map(|x| x.into())
                .unwrap_or(PathBuf::from("output.out"));

            match driver::run_link(&code, &library, &output) {
                Ok(()) => {
                    println!("linking successed.");
                }
                Err(err) => {
                    println!("{err}");
                    println!("linking failed!");
                }
            }
        }
        _ => unreachable!("Exhausted list of subcommands and subcommand_required prevents `None`"),
    }
}
