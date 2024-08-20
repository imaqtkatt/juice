use crate::{
  ast::{Arm, Expression, LitExpression, Operation},
  lexer::TokenKind,
};

use super::{ParseResult, Parser};

const PREC_TABLE: &[&[TokenKind]] = &[
  &[TokenKind::EqualsEquals],
  &[TokenKind::Plus, TokenKind::Minus],
  &[TokenKind::Star, TokenKind::Slash],
];

impl<'src> Parser<'src> {
  fn literal(&mut self) -> ParseResult<Expression> {
    match self.curr.kind {
      TokenKind::Identifier => self.expr_variable(),
      TokenKind::Number => self.expr_number(),
      TokenKind::String => self.expr_string(),
      TokenKind::Atom => self.expr_atom(),
      TokenKind::LBrace => self.expr_tuple(),
      TokenKind::LBracket => self.expr_list(),
      TokenKind::LParens => {
        self.expect(TokenKind::LParens)?;
        let expression = self.expression()?;
        self.expect(TokenKind::RParens)?;
        Ok(expression)
      }
      _ => self.unexpected(),
    }
  }

  fn expr_variable(&mut self) -> ParseResult<Expression> {
    let token = self.expect(TokenKind::Identifier)?;
    Ok(Expression::Var(token.lexeme))
  }

  fn expr_number(&mut self) -> ParseResult<Expression> {
    let token = self.expect(TokenKind::Number)?;
    Ok(Expression::Literal(LitExpression::Number(
      token.lexeme.parse().unwrap(),
    )))
  }

  fn expr_string(&mut self) -> ParseResult<Expression> {
    let token = self.expect(TokenKind::String)?;
    Ok(Expression::Literal(LitExpression::String(token.lexeme)))
  }

  fn expr_atom(&mut self) -> ParseResult<Expression> {
    let token = self.expect(TokenKind::Atom)?;
    Ok(Expression::Literal(LitExpression::Atom(token.lexeme)))
  }

  fn expr_tuple(&mut self) -> ParseResult<Expression> {
    self.expect(TokenKind::LBrace)?;
    let mut elements = vec![];
    while !self.is(TokenKind::RBrace) {
      elements.push(self.expression()?);
      if self.is(TokenKind::RBrace) {
        break;
      }
      self.expect(TokenKind::Comma)?;
    }
    self.expect(TokenKind::RBrace)?;
    Ok(Expression::Literal(LitExpression::Tuple(elements)))
  }

  fn expr_list(&mut self) -> ParseResult<Expression> {
    self.expect(TokenKind::LBracket)?;
    let mut elements = vec![];
    while !self.is(TokenKind::RBracket) {
      elements.push(self.expression()?);
      if self.is(TokenKind::RBracket) {
        break;
      }
      self.expect(TokenKind::Comma)?;
    }
    self.expect(TokenKind::RBracket)?;
    Ok(Expression::Literal(LitExpression::List(elements)))
  }

  fn call(&mut self) -> ParseResult<Expression> {
    let callee = self.literal()?;
    if self.is(TokenKind::LParens) {
      self.expect(TokenKind::LParens)?;
      let mut arguments = vec![];
      while !self.is(TokenKind::RParens) {
        arguments.push(self.expression()?);
        if self.is(TokenKind::RParens) {
          break;
        }
        self.expect(TokenKind::Comma)?;
      }
      self.expect(TokenKind::RParens)?;
      Ok(Expression::Call(Box::new(callee), arguments))
    } else {
      Ok(callee)
    }
  }

  fn infix(&mut self, precedence: usize) -> ParseResult<Expression> {
    if precedence > PREC_TABLE.len() - 1 {
      return self.call();
    }

    let mut left = self.infix(precedence + 1)?;
    while PREC_TABLE[precedence].iter().any(|a| self.is(*a)) {
      let operation = self.operation()?;
      let right = self.infix(precedence + 1)?;
      left = Expression::Binary(operation, Box::new(left), Box::new(right));
    }
    Ok(left)
  }

  fn operation(&mut self) -> ParseResult<Operation> {
    let operation = match self.curr.kind {
      TokenKind::Plus => Operation::Add,
      TokenKind::Minus => Operation::Sub,
      TokenKind::Star => Operation::Mul,
      TokenKind::Slash => Operation::Div,
      TokenKind::EqualsEquals => Operation::Eql,
      _ => return self.unexpected(),
    };
    self.eat();
    Ok(operation)
  }

  pub fn expression(&mut self) -> ParseResult<Expression> {
    match self.curr.kind {
      TokenKind::Case => self.case(),
      TokenKind::If => self.r#if(),
      TokenKind::Try => self.r#try(),
      _ => self.infix(0),
    }
  }

  fn case(&mut self) -> ParseResult<Expression> {
    self.expect(TokenKind::Case)?;
    let mut scrutinees = vec![self.expression()?];
    while self.is(TokenKind::Comma) {
      self.expect(TokenKind::Comma)?;
      scrutinees.push(self.expression()?);
    }
    self.expect(TokenKind::Of)?;
    self.expect(TokenKind::VBeg)?;

    let mut arms = vec![self.arm()?];
    while !self.is(TokenKind::VEnd) {
      arms.push(self.arm()?);
    }

    self.expect_or_pop_layout(TokenKind::VEnd)?;
    self.expect(TokenKind::End)?;
    Ok(Expression::Case(scrutinees, arms))
  }

  fn arm(&mut self) -> ParseResult<Arm> {
    let mut patterns = vec![self.pattern()?];
    while self.is(TokenKind::Comma) {
      self.expect(TokenKind::Comma)?;
      patterns.push(self.pattern()?);
    }
    self.expect(TokenKind::Arrow)?;
    let rhs = self.in_block(Self::r#let)?;
    Ok(Arm { lhs: patterns, rhs })
  }

  pub fn r#let(&mut self) -> ParseResult<Expression> {
    if self.curr.kind == TokenKind::Let {
      self.expect(TokenKind::Let)?;
      let lhs = self.pattern()?;
      self.expect(TokenKind::Equals)?;
      let rhs = self.expression()?;
      self.expect_one_of(&[TokenKind::VSep, TokenKind::Semicolon])?;
      let next = self.r#let()?;
      Ok(Expression::Assignment(lhs, Box::new(rhs), Box::new(next)))
    } else {
      self.expression()
    }
  }

  fn r#if(&mut self) -> ParseResult<Expression> {
    self.expect(TokenKind::If)?;
    let condition = self.expression()?;
    self.expect(TokenKind::Then)?;
    let then_branch = self.in_block(Self::r#let)?;
    self.expect(TokenKind::Else)?;
    let else_branch = self.in_block(Self::r#let)?;
    Ok(Expression::If(
      Box::new(condition),
      Box::new(then_branch),
      Box::new(else_branch),
    ))
  }

  fn r#try(&mut self) -> ParseResult<Expression> {
    self.expect(TokenKind::Try)?;
    let pat = self.pattern()?;
    self.expect(TokenKind::Equals)?;
    let expression = self.expression()?;
    self.expect(TokenKind::Then)?;
    let then_branch = self.in_block(Self::r#let)?;
    self.expect(TokenKind::Else)?;
    let else_branch = self.in_block(Self::r#let)?;
    Ok(Expression::Try(
      pat,
      Box::new(expression),
      Box::new(then_branch),
      Box::new(else_branch),
    ))
  }
}

#[cfg(test)]
mod test {
  use crate::{lexer::Lexer, parser::Parser};

  #[test]
  fn parse_expression() {
    let input = r#"
let tup = {1, 2};
case tup of
  {a, b} ->
    let res = a + b
    res == 3
  _ -> 2
end
"#;
    let mut parser = Parser::new(Lexer::new(input));
    let e = parser.r#let();
    match e {
      Ok(e) => println!("{e:?}"),
      Err(e) => eprintln!("{e}"),
    }
  }
}
