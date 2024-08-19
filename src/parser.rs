pub mod expression;
pub mod function;
pub mod pattern;
pub mod program;

use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser<'src> {
  curr: Token,
  next: Token,
  lexer: Lexer<'src>,
}

impl<'src> Parser<'src> {
  pub fn new(mut lexer: Lexer<'src>) -> Self {
    let curr = lexer.next_token();
    let next = lexer.next_token();
    Self { curr, next, lexer }
  }
}

pub type ParseResult<T> = std::result::Result<T, String>;

impl<'src> Parser<'src> {
  pub fn is(&self, token_kind: TokenKind) -> bool {
    self.curr.kind == token_kind
  }

  pub fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> {
    if self.curr.kind == expected {
      Ok(self.eat())
    } else {
      Err(format!(
        "Expected '{:?}' but found '{}'",
        expected, self.curr.lexeme
      ))
    }
  }

  pub fn expect_one_of(&mut self, kinds: &[TokenKind]) -> ParseResult<Token> {
    for kind in kinds {
      if self.curr.kind == *kind {
        return Ok(self.eat());
      } else {
        continue;
      }
    }
    Err(format!("Unexpected '{}'", self.curr.lexeme))
  }

  pub fn eat(&mut self) -> Token {
    let new_curr = std::mem::replace(&mut self.next, self.lexer.next_token());
    std::mem::replace(&mut self.curr, new_curr)
  }

  pub fn in_block<R>(&mut self, p: impl Fn(&mut Self) -> ParseResult<R>) -> ParseResult<R> {
    self.expect(TokenKind::VBeg)?;
    let res = p(self)?;
    self.expect_or_pop_layout(TokenKind::VEnd)?;
    // self.expect(TokenKind::VEnd)?;
    Ok(res)
  }

  pub fn expect_or_pop_layout(&mut self, kind: TokenKind) -> ParseResult<()> {
    if self.curr.kind == kind {
      self.eat();
    } else {
      self.lexer.pop_layout();
    }
    Ok(())
  }
}
