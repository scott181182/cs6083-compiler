use crate::parser::ProgramNode;

use self::declaration::AnalyzedDeclaration;
use self::util::{Context, Scope};



pub mod declaration;
// pub mod misc;
// pub mod procedure;
// pub mod statement;
// pub mod expression;
pub mod util;



#[derive(Debug)]
pub struct AnalyzedProgram {
    name: String,
    declarations: Vec<declaration::AnalyzedDeclaration>,
    // statements: Vec<statement::AnalyzedStatement>
}
impl AnalyzedProgram {
    pub fn analyze(value: ProgramNode) -> Result<Self, util::SemanticError> {
        let ctx = Context::new();

        let name = value.header.ident;
        let declarations = Vec::new();
        for decl in value.body.declarations {
            declarations.push(decl.analyze(&mut ctx, Scope::Global)?);
        }

        // let statements = Vec::new();
        // for decl in value.body.declarations {
        //     statements.push(AnalyzedStatement::analyze(decl, &mut ctx, Scope::Global)?);
        // }

        Ok(AnalyzedProgram { name, declarations })
        // Ok(AnalyzedProgram { name, declarations, statements })
    }
}
