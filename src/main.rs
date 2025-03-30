use tokens::Token;
pub mod ast;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod tokens;
fn main() {
    loop {
        let buffer = {
            let mut line = String::new();
            std::io::stdin().read_line(&mut line).unwrap();
            line
        };

        if buffer == "exit\n" {
            break;
        } else {
            let l = lexer::Lexer::new(&buffer);
            let mut p = parser::Parser::new(l);
            let program = p.parse_program();
            println!(">>> {:?}", program);
        }
    }
}
