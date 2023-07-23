use std::{env, fs, io};
use std::path::PathBuf;
use std::process::exit;

use thiserror::Error;



mod lexer;



#[derive(Error, Debug)]
enum ProgramError {
    #[error(transparent)]
    ArgumentError(#[from] ArgumentError),
    #[error(transparent)]
    FileError(#[from] io::Error),
    #[error(transparent)]
    LexerError(#[from] lexer::LexerError)
}

#[derive(Error, Debug)]
enum ArgumentError {
    #[error("Expected at least one file argument to compile, but found none!")]
    NoArguments,
    #[error("Expected input file and optional output file argument, but found more than 2 arguments!")]
    TooManyArguments,
    #[error("Input file does not exist")]
    FileDoesNotExist
}

fn parse_args() -> Result<(PathBuf, PathBuf), ArgumentError> {
    let mut args = env::args().skip(1);
    let input_filename_opt = args.next();
    let output_filename_opt = args.next();

    let input_path = match input_filename_opt {
        None => return Err(ArgumentError::NoArguments),
        Some(input_filename) => PathBuf::from(input_filename)
    };
    if args.len() > 0 { return Err(ArgumentError::TooManyArguments); }
    if !input_path.is_file() { return Err(ArgumentError::FileDoesNotExist); }
    
    let output_path = match output_filename_opt {
        Some(output_filename) => {
            let mut output_path = PathBuf::from(output_filename);
            if output_path.is_dir() {
                // This can be unwrapped, since we checked that `input_path` is a file earlier.
                output_path.push(input_path.file_name().unwrap());
            }
            output_path
        },
        None => {
            let mut output_path = input_path.clone();
            output_path.set_extension("out");
            output_path
        }
    };

    Ok((input_path, output_path))
}



fn run_program() -> Result<(), ProgramError> {
    let (input_path, output_path) = parse_args()?;

    let input_data = fs::read_to_string(&input_path)?;
    let toks = lexer::lex(input_data)?;
    println!("{:?}", toks);

    Ok(())
}

fn main() {
    let res = run_program();
    if let Err(err) = res {
        eprintln!("{}", err);
        exit(1);
    }
}
