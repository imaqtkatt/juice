use crate::{
  ast::{LitPattern, Pattern},
  lexer::TokenKind,
};

use super::{ParseResult, Parser};

impl<'src> Parser<'src> {
  pub fn pattern(&mut self) -> ParseResult<Pattern> {
    match self.curr.kind {
      TokenKind::Wildcard => self.pat_wildcard(),
      TokenKind::Identifier => self.pat_variable(),
      TokenKind::Number => self.pat_number(),
      TokenKind::String => self.pat_string(),
      TokenKind::Atom => self.pat_atom(),
      TokenKind::LBrace => self.pat_tuple(),
      TokenKind::LBracket => self.pat_list(),
      _ => self.unexpected(),
    }
  }

  fn pat_wildcard(&mut self) -> ParseResult<Pattern> {
    self.expect(TokenKind::Wildcard)?;
    Ok(Pattern::Wildcard)
  }

  fn pat_variable(&mut self) -> ParseResult<Pattern> {
    let token = self.expect(TokenKind::Identifier)?;
    Ok(Pattern::Var(token.lexeme))
  }

  fn pat_number(&mut self) -> ParseResult<Pattern> {
    let token = self.expect(TokenKind::Number)?;
    Ok(Pattern::Literal(LitPattern::Number(
      token.lexeme.parse().unwrap(),
    )))
  }

  fn pat_string(&mut self) -> ParseResult<Pattern> {
    let token = self.expect(TokenKind::String)?;
    let mut tail = None;
    if self.is(TokenKind::LtGt) {
      self.expect(TokenKind::LtGt)?;
      tail = Some(Box::new(self.pattern()?));
    }
    Ok(Pattern::Literal(LitPattern::String(token.lexeme, tail)))
  }

  fn pat_atom(&mut self) -> ParseResult<Pattern> {
    let token = self.expect(TokenKind::Atom)?;
    Ok(Pattern::Literal(LitPattern::Atom(token.lexeme)))
  }

  fn pat_tuple(&mut self) -> ParseResult<Pattern> {
    self.expect(TokenKind::LBrace)?;
    let mut elements = vec![];
    while !self.is(TokenKind::RBrace) {
      elements.push(self.pattern()?);
      if self.is(TokenKind::RBrace) {
        break;
      }
      self.expect(TokenKind::Comma)?;
    }
    self.expect(TokenKind::RBrace)?;
    Ok(Pattern::Literal(LitPattern::Tuple(elements)))
  }

  fn pat_list(&mut self) -> ParseResult<Pattern> {
    self.expect(TokenKind::LBracket)?;
    let mut elements = vec![];
    let mut has_tail = false;
    while !self.is(TokenKind::RBracket) {
      elements.push(self.pattern()?);
      if self.is(TokenKind::RBracket) {
        break;
      }
      if self.is(TokenKind::Pipe) {
        self.expect(TokenKind::Pipe)?;
        has_tail = true;
        break;
      }
      self.expect(TokenKind::Comma)?;
    }
    let tail = if has_tail {
      Some(Box::new(self.pattern()?))
    } else {
      None
    };
    self.expect(TokenKind::RBracket)?;
    Ok(Pattern::Literal(LitPattern::List(elements, tail)))
  }
}
