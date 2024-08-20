use crate::{ast::Module, lexer::TokenKind};

use super::{ParseResult, Parser};

impl<'src> Parser<'src> {
  pub fn program(&mut self) -> ParseResult<Module> {
    self.expect(TokenKind::Module)?;
    let module_name = self.expect(TokenKind::Identifier)?;
    let name = module_name.lexeme;

    let mut fun_definitions = indexmap::IndexMap::new();
    while !self.is(TokenKind::Eof) {
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
    self.expect(TokenKind::Eof)?;
    Ok(Module {
      name,
      fun_definitions,
    })
  }
}

#[cfg(test)]
mod test {
  use crate::{lexer::Lexer, parser::Parser};

  #[test]
  fn parse_program() {
    let input = r#"
module main

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
module main
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
module main
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
