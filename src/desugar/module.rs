use crate::ast;

use super::{Desugar, Module};

impl Desugar for ast::Module {
  type Out = Module;

  type Err = String;

  fn desugar(self) -> Result<Self::Out, Self::Err> {
    let mut fun_definitions = indexmap::IndexMap::new();
    for (name, def) in self.fun_definitions.into_iter() {
      fun_definitions.insert(name, def.desugar()?);
    }
    Ok(Module {
      name: self.name,
      fun_definitions,
    })
  }
}

#[cfg(test)]
mod test {
  use crate::{desugar::Desugar, lexer::Lexer, parser::Parser, report};

  #[test]
  fn desugar_module() {
    let input = r#"
module desugar

fun batata("oi" <> a) do
  a
fun batata("teste") do
  #ok
fun batata(_) do
  #err
end
"#;
    let mut parser = Parser::new(Lexer::new(input));
    let module = parser.program();
    let module = module.unwrap();
    let (reporter, receiver) = report::Reporter::new();
    match module.desugar() {
      Ok(module) => {
        module.check_unbound_variables(&reporter);
        println!("{module:?}");
        report::Reporter::to_stdout(receiver);
      }
      Err(e) => {
        report::Reporter::to_stdout(receiver);
        eprintln!("{e}")
      }
    }
  }
}
