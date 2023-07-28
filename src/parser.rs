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