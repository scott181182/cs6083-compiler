use crate::lexer::TokenStream;
pub use util::{ParserError, ParseTokens};
pub use program::*;


mod declaration;
mod misc;
mod procedure;
mod statement;
mod program;
mod expression;
mod util;




pub fn parse(mut toks: TokenStream) -> Result<ProgramNode, ParserError> {
    ProgramNode::parse(&mut toks)
}