use std::collections::VecDeque;

use crate::lexer::Token;
pub use self::util::{ParserError, ParseTokens, TokenStream};
pub use program::*;



mod declaration;
mod misc;
mod procedure;
mod statement;
mod program;
mod expression;
mod util;




pub fn parse(mut toks: VecDeque<Token>) -> Result<ProgramNode, ParserError> {
    let mut stream = TokenStream::new(toks);
    ProgramNode::parse(&mut stream)
}