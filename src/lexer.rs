use std::collections::VecDeque;

use thiserror::Error;



mod preprocess;



#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unknown symbol code(s): {:?}", .0.bytes().collect::<Vec<u8>>())]
    UnknownSymbol(String)
}



#[derive(Debug, PartialEq, Clone)]
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

impl Token {
    pub fn from_identifier(value: String) -> Self {
        let value = value.to_lowercase();
        match value.as_str() {
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
            // Identifiers a case-insensitive, so lowercase!
            _ => Token::Identifier(value)
        }
    }

    pub fn from_symbol(value: &str) -> Result<Self, LexerError> {
        match value {
            "."  => Ok(Token::Period),
            ";"  => Ok(Token::Semicolon),
            ":"  => Ok(Token::Colon),
            ","  => Ok(Token::Comma),
            "("  => Ok(Token::LeftParen),
            ")"  => Ok(Token::RightParen),
            "["  => Ok(Token::LeftBracket),
            "]"  => Ok(Token::RightBracket),
            ":=" => Ok(Token::Assign),
            "&"  => Ok(Token::Ampersand),
            "|"  => Ok(Token::Bar),
            "+"  => Ok(Token::Plus),
            "-"  => Ok(Token::Minus),
            "<"  => Ok(Token::LessThan),
            "<=" => Ok(Token::LessThanEq),
            ">"  => Ok(Token::GreaterThan),
            ">=" => Ok(Token::GreaterThanEq),
            "==" => Ok(Token::DoubleEqual),
            "!=" => Ok(Token::BangEqual),
            "*"  => Ok(Token::Asterisk),
            "/"  => Ok(Token::Slash),
            _ => Err(LexerError::UnknownSymbol(value.to_owned()))
        }
    }
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



pub fn lex(raw_content: String) -> Result<VecDeque<Token>, LexerError> {
    let content = preprocess::strip_comments(raw_content);

    let mut toks = VecDeque::new();
    let mut partial = PartialToken::None;

    for c in content.chars() {
        partial = match (partial, c) {
            (PartialToken::None, ' ' | '\t' | '\n') => PartialToken::None,
            (PartialToken::Identifier(p), ' ' | '\t' | '\n') => {
                toks.push_back(Token::from_identifier(p));
                PartialToken::None
            },
            (PartialToken::NumberLiteral(p), ' ' | '\t' | '\n') => {
                toks.push_back(Token::NumberLiteral(p));
                PartialToken::None
            },
            (PartialToken::Symbol(p), ' ' | '\t' | '\n') => {
                toks.push_back(Token::from_symbol(&p)?);
                PartialToken::None
            },


            (PartialToken::None, cin) => PartialToken::try_from(cin)?,

            (PartialToken::Identifier(mut p), cin) if cin.is_alphanumeric() || cin == '_' => {
                p.push(cin);
                PartialToken::Identifier(p)
            },
            (PartialToken::Identifier(p), _) => {
                toks.push_back(Token::from_identifier(p));
                PartialToken::try_from(c)?
            },

            (PartialToken::NumberLiteral(mut p), cin) if cin.is_numeric() || cin == '_' => {
                p.push(cin);
                PartialToken::NumberLiteral(p)
            },
            (PartialToken::NumberLiteral(mut p), '.') => {
                if p.contains(".") {
                    // This number literal already has a decimal point, so assume this is another symbol
                    toks.push_back(Token::NumberLiteral(p));
                    PartialToken::try_from(c)?
                } else {
                    p.push(c);
                    PartialToken::NumberLiteral(p)
                }
            },
            (PartialToken::NumberLiteral(p), _) => {
                toks.push_back(Token::NumberLiteral(p));
                PartialToken::try_from(c)?
            },

            (PartialToken::StringLiteral(p), '"') => {
                toks.push_back(Token::StringLiteral(p));
                PartialToken::None
            },
            (PartialToken::StringLiteral(mut p), _) => {
                p.push(c);
                PartialToken::StringLiteral(p)
            },

            (PartialToken::Symbol(mut p), _) => {
                if MONO_SYMBOLS.contains(&p) {
                    toks.push_back(Token::from_symbol(&p)?);
                    PartialToken::try_from(c)?
                } else {
                    if p == "<" && c == '=' {
                        toks.push_back(Token::LessThanEq);
                        PartialToken::None
                    } else if p == ">" && c == '=' {
                        toks.push_back(Token::GreaterThanEq);
                        PartialToken::None
                    } else if p == ":" {
                        if c == '=' {
                            toks.push_back(Token::Assign);
                            PartialToken::None
                        } else {
                            toks.push_back(Token::Colon);
                            PartialToken::try_from(c)?
                        }
                    } else if p == "=" && c == '=' {
                        toks.push_back(Token::DoubleEqual);
                        PartialToken::None
                    } else if p == "!" && c == '=' {
                        toks.push_back(Token::BangEqual);
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




#[cfg(test)]
mod tests {
    use super::LexerError;

    #[test]
    fn incorrect_test2() -> Result<(), crate::ProgramError> {
        let input_data = std::fs::read_to_string("test_programs/incorrect/test2.src")?;
        match crate::lexer::lex(input_data) {
            Ok(_) => panic!("Expected incorrect test1 to error, but it succeeded"),
            Err(LexerError::UnknownSymbol(sym)) if sym == "#s" => Ok(()),
            _ => panic!("Incorrect test1 failed, but in an unexpected way")
        }
    }
}
