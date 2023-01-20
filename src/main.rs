pub mod ast;
pub mod intern;
pub mod lexer;
pub mod parser;
pub mod position;
pub mod printer;
pub mod env_map;
pub mod renamer;
pub mod infer;
pub mod diagnostic;
pub mod anf;
pub mod visitor;
pub mod normalize;
// pub mod simple_opt;
// pub mod clos_conv;
// pub mod codegen;

fn main() {
    println!("Hello, world!");
}
