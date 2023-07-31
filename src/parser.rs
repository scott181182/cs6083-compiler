use std::collections::VecDeque;

use crate::lexer::Token;
pub use self::util::{ParserError, ParseTokens, TokenStream};
pub use program::*;



pub mod declaration;
pub mod misc;
pub mod procedure;
pub mod statement;
pub mod program;
pub mod expression;
pub mod util;




pub fn parse(toks: VecDeque<Token>) -> Result<ProgramNode, ParserError> {
    let mut stream = TokenStream::new(toks);
    ProgramNode::parse(&mut stream)
}




#[cfg(test)]
mod tests {
    use super::ParserError;
    use crate::lexer::Token;
    
    #[test]
    fn incorrect_test1bt() -> Result<(), crate::ProgramError> {
        let input_data = std::fs::read_to_string("test_programs/incorrect/test1b.src~")?;
        let toks = crate::lexer::lex(input_data)?;
        match crate::parser::parse(toks) {
            Ok(_) => panic!("Expected incorrect test1 to error, but it succeeded"),
            Err(ParserError::UnexpectedToken(tok, Token::Global)) if tok == "Semicolon" => Ok(()),
            _ => panic!("Incorrect test1 failed, but in an unexpected way")
        }
    }
}
