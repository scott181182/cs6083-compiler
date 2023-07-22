use thiserror::Error;



mod preprocess;



#[derive(Error, Debug)]
pub enum LexerError {

}

pub enum Token {

}

pub fn lex(raw_content: String) -> Result<Vec<Token>, LexerError> {
    let content = preprocess::strip_comments(raw_content);

    println!("{}", content);

    let toks = Vec::new();



    Ok(toks)
}