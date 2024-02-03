pub mod lexer;
pub mod repl;
pub mod token;

fn main() {
    println!("Radon Programming Language");
    println!("Version 0.1.0");
    println!("__________________________");
    repl::start();
}
