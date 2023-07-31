use crate::parser::ProgramNode;

use self::procedure::AnalyzedProcedure;
use self::util::{Context, Scope, Analyze, ScopeContext};



pub mod declaration;
pub mod error;
pub mod procedure;
pub mod statement;
pub mod expression;
pub mod util;



#[derive(Debug)]
pub struct AnalyzedProgram {
    pub name: String,
    pub declarations: ScopeContext,
    pub procedures: Vec<AnalyzedProcedure>,
    pub block: statement::AnalyzedBlock
}
impl AnalyzedProgram {
    pub fn analyze(value: ProgramNode) -> Result<Self, error::SemanticError> {
        let mut ctx = Context::new();

        let name = value.header.ident;
        let mut procedures = Vec::new();
        for decl in value.body.declarations {
            if let Some(proc) = decl.analyze(&mut ctx, &Scope::Global)? {
                procedures.push(proc);
            }
        }

        let block = value.body.statements.analyze(&mut ctx, &Scope::Local)?;

        Ok(AnalyzedProgram { name, declarations: ctx.into_global(), procedures, block })
    }
}



#[cfg(test)]
mod tests {
    use rstest::rstest;
    use std::path::PathBuf;

    use super::error::SemanticError;
    


    #[rstest]
    fn compile_test_correct(
        #[files("test_programs/correct/*.src")] source_file: PathBuf,
    ) -> Result<(), crate::ProgramError> {
        let input_data = std::fs::read_to_string(&source_file)?;
        let toks = crate::lexer::lex(input_data)?;
        // println!("{:?}", toks);
        let program = crate::parser::parse(toks)?;
        let _analyzed_program = crate::analyzer::AnalyzedProgram::analyze(program)?;
    
        Ok(())
    }

    #[test]
    fn incorrect_test1() -> Result<(), crate::ProgramError> {
        let input_data = std::fs::read_to_string("test_programs/incorrect/test1.src")?;
        let toks = crate::lexer::lex(input_data)?;
        let program = crate::parser::parse(toks)?;

        match crate::analyzer::AnalyzedProgram::analyze(program) {
            Ok(_) => panic!("Expected incorrect test1 to error, but it succeeded"),
            Err(SemanticError::Redeclaration(ident)) if ident == "zach" => Ok(()),
            _ => panic!("Incorrect test1 failed, but in an unexpected way")
        }
    }
}
