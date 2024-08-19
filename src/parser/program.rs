use crate::ast::Program;

use super::{ParseResult, Parser};

impl<'src> Parser<'src> {
  pub fn program(&mut self) -> ParseResult<Program> {
    let mut fun_definitions = indexmap::IndexMap::new();
    while !self.is(crate::lexer::TokenKind::Eof) {
      let fun_definition = self.fun_definition()?;
      match fun_definitions.entry(fun_definition.name.clone()) {
        indexmap::map::Entry::Occupied(_) => {
          return Err(format!(
            "Redefinition of function '{}'.",
            fun_definition.name
          ));
        }
        indexmap::map::Entry::Vacant(v) => {
          v.insert(fun_definition);
        }
      }
    }
    self.expect(crate::lexer::TokenKind::Eof)?;
    Ok(Program { fun_definitions })
  }
}

#[cfg(test)]
mod test {
  use crate::{lexer::Lexer, parser::Parser};

  #[test]
  fn parse_program() {
    let input = r#"
fun main() do
  batata({1, 2})
end

fun batata({a, b}) do
  a + b
fun batata(_) do
  #error
end
"#;
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.program();
    match program {
      Ok(program) => println!("{program:?}"),
      Err(e) => eprintln!("{e}"),
    }
  }

  #[test]
  fn parse_redefinition() {
    let input = r#"
fun main() do
  batata({1, 2})
end

fun main() do
  #ok
end
"#;
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.program();
    match program {
      Ok(program) => println!("{program:?}"),
      Err(e) => eprintln!("{e}"),
    }
  }

  #[test]
  fn parse_wrong_clause() {
    let input = r#"
fun foo() do
  1
fun bar() do
  #err
end
"#;
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.program();
    match program {
      Ok(program) => println!("{program:?}"),
      Err(e) => eprintln!("{e}"),
    }
  }
}
