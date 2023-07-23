use crate::lexer::TokenStream;
use util::ParserError;

use self::{nodes::ProgramNode, parsing::ParseTokens};



mod nodes;
mod parsing;
pub mod util;



pub fn parse(mut toks: TokenStream) -> Result<ProgramNode, ParserError> {
    ProgramNode::parse(&mut toks)
}