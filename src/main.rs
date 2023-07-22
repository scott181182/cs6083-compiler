use std::env;
use std::path::PathBuf;
use std::process::exit;

use thiserror::Error;



mod lexer;



#[derive(Error, Debug)]
enum ArgumentError {
    #[error("Expected at least one file argument to compile, but found none!")]
    NoArguments,
    #[error("Expected input file and optional output file argument, but found more than 2 arguments!")]
    TooManyArguments
}

fn parse_args() -> Result<(PathBuf, PathBuf), ArgumentError> {
    let mut args = env::args().skip(1);
    let input_filename_opt = args.next();
    let output_filename_opt = args.next();

    if input_filename_opt.is_none() { return Err(ArgumentError::NoArguments); }
    if let Some(_) = args.next() { return Err(ArgumentError::TooManyArguments); }

    let input_path = PathBuf::from(input_filename_opt.unwrap());
    let output_path = match output_filename_opt {
        Some(output_filename) => {
            let mut output_path = PathBuf::from(output_filename);
            if output_path.is_dir() {
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



fn run_program() -> Result<(), ArgumentError> {
    let (input_path, output_path) = parse_args()?;

    println!("{:?}, {:?}", input_path, output_path);

    Ok(())
}

fn main() {
    let res = run_program();
    if let Err(err) = res {
        eprintln!("{}", err);
        exit(1);
    }
}
