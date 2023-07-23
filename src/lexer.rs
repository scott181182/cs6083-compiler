use thiserror::Error;



mod preprocess;



#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unknown symbol: {0}")]
    UnknownSymbol(String)
}

// pub struct Location {
//     pub line_no: u64,
//     pub line_col: u64,
// }

#[derive(Debug)]
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

#[derive(Debug, PartialEq)]
enum TokenHint {
    Identifier, Numeric, StringLiteral, Symbol, None
}

enum PartialToken {
    None,
    Identifier(String),
    NumberLiteral(String),
    StringLiteral(String),
    Symbol(String)
}

impl TryFrom<char> for PartialToken {
    type Error = LexerError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            cin if cin.is_alphabetic() => Ok(PartialToken::Identifier(cin.to_string())),
            cin if cin.is_numeric() => Ok(PartialToken::NumberLiteral(cin.to_string())),
            '"' => Ok(PartialToken::StringLiteral(String::new())),
            // TODO: check against actual list of symbols.
            cin => Ok(PartialToken::Symbol(cin.to_string())),
        }
    }
}



const MONO_SYMBOLS: &'static str = ".;,()[]&|+-*/";

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
        "bool" => Token::Boolean,
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
                TokenHint::Numeric => Token::NumberLiteral(content.to_owned()),
                _ => panic!("Wow, an unexpected token!")
            }
        }
    }
}



pub fn lex(raw_content: String) -> Result<Vec<Token>, LexerError> {
    let content = preprocess::strip_comments(raw_content);

    let mut toks = Vec::new();
    let mut partial = PartialToken::None;
    let mut hint = TokenHint::None;

    for c in content.chars() {
        partial = match (partial, c) {
            (PartialToken::None, ' ' | '\t' | '\n') => PartialToken::None,
            (PartialToken::Identifier(p), ' ' | '\t' | '\n') => {
                toks.push(finish_partial(&p, &TokenHint::Identifier));
                PartialToken::None
            },
            (PartialToken::NumberLiteral(p), ' ' | '\t' | '\n') => {
                toks.push(finish_partial(&p, &TokenHint::Numeric));
                PartialToken::None
            },
            (PartialToken::Symbol(p), ' ' | '\t' | '\n') => {
                toks.push(finish_partial(&p, &TokenHint::Symbol));
                PartialToken::None
            },


            (PartialToken::None, cin) => PartialToken::try_from(cin)?,

            (PartialToken::Identifier(mut p), cin) if cin.is_alphanumeric() => {
                p.push(cin);
                PartialToken::Identifier(p)
            },
            (PartialToken::Identifier(p), _) => {
                toks.push(finish_partial(&p, &TokenHint::Identifier));
                PartialToken::try_from(c)?
            },

            (PartialToken::NumberLiteral(mut p), cin) if cin.is_numeric() => {
                p.push(cin);
                PartialToken::NumberLiteral(p)
            },
            (PartialToken::NumberLiteral(mut p), '.') => {
                if p.contains(".") {
                    // This number literal already has a decimal point, so assume this is another symbol
                    toks.push(finish_partial(&p, &TokenHint::Numeric));
                    PartialToken::try_from(c)?
                } else {
                    p.push(c);
                    PartialToken::NumberLiteral(p)
                }
            },
            (PartialToken::NumberLiteral(p), _) => {
                toks.push(finish_partial(&p, &TokenHint::Identifier));
                PartialToken::try_from(c)?
            },

            (PartialToken::StringLiteral(p), '"') => {
                toks.push(Token::StringLiteral(p));
                PartialToken::None
            },
            (PartialToken::StringLiteral(mut p), _) => {
                p.push(c);
                PartialToken::StringLiteral(p)
            },

            (PartialToken::Symbol(mut p), _) => {
                if MONO_SYMBOLS.contains(&p) {
                    toks.push(finish_partial(&p, &TokenHint::Symbol));
                    PartialToken::try_from(c)?
                } else {
                    if p == "<" && c == '=' {
                        toks.push(Token::LessThanEq);
                        PartialToken::None
                    } else if p == ">" && c == '=' {
                        toks.push(Token::GreaterThanEq);
                        PartialToken::None
                    } else if p == ":" {
                        if c == '=' {
                            toks.push(Token::Assign);
                            PartialToken::None
                        } else {
                            toks.push(Token::Colon);
                            PartialToken::try_from(c)?
                        }
                    } else if p == "=" && c == '=' {
                        toks.push(Token::DoubleEqual);
                        PartialToken::None
                    } else if p == "!" && c == '=' {
                        toks.push(Token::BangEqual);
                        PartialToken::None
                    } else {
                        p.push(c);
                        return Err(LexerError::UnknownSymbol(p))
                    }
                }
            }
        }
    }

    Ok(toks)
}