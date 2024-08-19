use crate::{
  ast::{FunClause, FunDefinition},
  lexer::TokenKind,
};

use super::{ParseResult, Parser};

impl<'src> Parser<'src> {
  pub fn fun_definition(&mut self) -> ParseResult<FunDefinition> {
    self.expect(TokenKind::Fun)?;
    let ident = self.expect(TokenKind::Identifier)?;
    let name = ident.lexeme;
    let mut clauses = vec![self.fun_clause()?];
    while !self.is(TokenKind::End) {
      self.expect(TokenKind::Fun)?;
      let ident = self.expect(TokenKind::Identifier)?;
      if ident.lexeme != name {
        return Err(format!(
          "Clause definition error, names {} and {} are not equal.",
          name, ident.lexeme
        ));
      } else {
        clauses.push(self.fun_clause()?);
      }
    }
    self.expect(TokenKind::End)?;
    Ok(FunDefinition { name, clauses })
  }

  fn fun_clause(&mut self) -> ParseResult<FunClause> {
    self.expect(TokenKind::LParens)?;
    let mut patterns = vec![];
    while !self.is(TokenKind::RParens) {
      patterns.push(self.pattern()?);
      if self.is(TokenKind::RParens) {
        break;
      }
      self.expect(TokenKind::Comma)?;
    }
    self.expect(TokenKind::RParens)?;
    self.expect(TokenKind::Do)?;
    let body = self.in_block(Self::expression)?;
    Ok(FunClause { patterns, body })
  }
}

#[cfg(test)]
mod test {
  use crate::{lexer::Lexer, parser::Parser};

  #[test]
  fn parse_fun_definition() {
    let input = r#"
fun oi(1) do
  2
// chocolate
fun oi(a) do
  case a of
    {a, b} -> a + b
    _ -> a
  end
end
"#;
    let mut parser = Parser::new(Lexer::new(input));
    let fun_def = parser.fun_definition();
    match fun_def {
      Ok(fun) => println!("{fun:?}"),
      Err(e) => eprintln!("{e}"),
    }
  }
}
