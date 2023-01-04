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
pub mod normalize;
pub mod simple_opt;

fn main() {
    println!("Hello, world!");
}
