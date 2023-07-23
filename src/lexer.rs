use thiserror::Error;



mod preprocess;



#[derive(Error, Debug)]
pub enum LexerError {

}

// pub struct Location {
//     pub line_no: u64,
//     pub line_col: u64,
// }

pub enum Token {
    // Keywords
    Program,
    Is,
    Begin,
    End,
    Global,
    Procedure,
    Variable,
    Integer,
    Float,
    String,
    Boolean,
    If,
    Then,
    Else,
    For,
    Return,
    Not,
    True,
    False,

    // Symbols
    Period,
    Semicolon,
    Colon,
    Comma,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Assign,
    Ampersand,
    Bar,
    Plus,
    Minus,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    DoubleEqual,
    BangEqual,
    Asterisk,
    Slash,

    // Variables
    Identifier(String),
    NumberLiteral(String),
    StringLiteral(String)
}

enum TokenHint {
    Identifier, NumberLiteral, StringLiteral, Symbol, Keyword, None
}

fn finish_partial(content: &str, hint: &TokenHint) -> Token {
    match content.to_lowercase().as_str() {
        "program" => Token::Program,
        "is" => Token::Is,
        "begin" => Token::Begin,
        "end" => Token::End,
        "global" => Token::Global,
        "procedure" => Token::Procedure,
        "variable" => Token::Variable,
        "integer" => Token::Integer,
        "float" => Token::Float,
        "string" => Token::String,
        "boolean" => Token::Boolean,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "for" => Token::For,
        "return" => Token::Return,
        "not" => Token::Not,
        "true" => Token::True,
        "false" => Token::False,

        "." => Token::Period,
        ";" => Token::Semicolon,
        ":" => Token::Colon,
        "," => Token::Comma,
        "(" => Token::LeftParen,
        ")" => Token::RightParen,
        "[" => Token::LeftBracket,
        "]" => Token::RightBracket,
        ":=" => Token::Assign,
        "&" => Token::Ampersand,
        "|" => Token::Bar,
        "+" => Token::Plus,
        "-" => Token::Minus,
        "<" => Token::LessThan,
        "<=" => Token::LessThanEq,
        ">" => Token::GreaterThan,
        ">=" => Token::GreaterThanEq,
        "==" => Token::DoubleEqual,
        "!=" => Token::BangEqual,
        "*" => Token::Asterisk,
        "/" => Token::Slash,

        _ => {
            if content.starts_with('"') && content.ends_with('"') {
                return Token::StringLiteral(content.replace("\"", ""));
            }
            match hint {
                TokenHint::Identifier => Token::Identifier(content.to_owned()),
                TokenHint::NumberLiteral => Token::NumberLiteral(content.to_owned()),
                _ => panic!("Wow, an unexpected token!")
            }
        }
    }
}

macro_rules! process_token {
    ($toks: ident, $partial: ident, $hint: ident) => {
        $toks.push(finish_partial(&$partial, &$hint));
        $partial.clear();
        continue
    };
}

pub fn lex(raw_content: String) -> Result<Vec<Token>, LexerError> {
    let content = preprocess::strip_comments(raw_content);

    println!("{}", content);

    let mut toks = Vec::new();
    let mut partial = String::new();
    let mut hint = TokenHint::None;

    for c in content.chars() {
        if c == ' ' || c == '\t' || c == '\n' {
            process_token!(toks, partial, hint);
        }

        match &partial {
            _ => {}
        }

        if c.is_alphanumeric() {
            partial.push(c);
        }
    }

    Ok(toks)
}