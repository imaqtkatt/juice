use std::{iter::Peekable, str::Chars};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
  VBeg,
  VEnd,
  VSep,
  LParens,
  RParens,
  LBracket,
  RBracket,
  LBrace,
  RBrace,
  Wildcard,
  Comma,
  Semicolon,
  Pipe,
  Plus,
  Minus,
  Star,
  Slash,
  Equals,
  EqualsEquals,
  LtGt,
  Identifier,
  Number,
  String,
  Atom,
  Fun,
  Do,
  Case,
  Of,
  End,
  Let,
  Arrow,
  Error,
  Eof,
}

#[derive(Debug)]
pub struct Token {
  pub kind: TokenKind,
  pub lexeme: String,
}

pub enum LexerState {
  Default,
  Push,
}

pub struct Lexer<'src> {
  input: &'src str,
  peekable: Peekable<Chars<'src>>,
  start: usize,
  index: usize,
  column: usize,
  line: usize,
  layout: Vec<usize>,
  state: LexerState,
}

impl<'src> Lexer<'src> {
  pub fn new(input: &'src str) -> Self {
    Self {
      input,
      peekable: input.chars().peekable(),
      start: 0,
      index: 0,
      column: 0,
      line: 0,
      layout: vec![],
      state: LexerState::Default,
    }
  }

  fn advance(&mut self) -> Option<char> {
    let char = self.peekable.next()?;
    self.index += char.len_utf8();

    self.column += 1;

    if char == '\n' {
      self.column = 0;
      self.line += 1;
    }

    Some(char)
  }

  fn advance_while(&mut self, predicate: impl Fn(&char) -> bool) {
    while let Some(char) = self.peekable.peek() {
      if predicate(char) {
        self.advance();
      } else {
        break;
      }
    }
  }

  fn skip_whitespaces(&mut self) {
    self.start = self.index;
    self.advance_while(|c| c.is_ascii_whitespace());
  }

  fn skip_comment(&mut self) {
    let mut clone = self.peekable.clone();
    clone.next();
    if let Some(('/', '/')) = self.peekable.peek().zip(clone.peek()) {
      self.advance_while(|c| *c != '\n')
    } else {
    }
  }

  fn skip(&mut self) {
    loop {
      match self.peekable.peek() {
        Some(char) => {
          if char.is_ascii_whitespace() {
            self.skip_whitespaces();
          } else if *char == '/' {
            self.skip_comment();
          } else {
            break;
          }
        }
        None => break,
      }
    }
  }

  fn string(&mut self) -> (TokenKind, String) {
    let mut s = String::new();
    while let Some(char) = self.peekable.peek() {
      if *char == '"' {
        break;
      } else {
        s.push(self.advance().unwrap());
      }
    }
    if let Some('"') = self.advance() {
      (TokenKind::String, s)
    } else {
      (TokenKind::Error, s)
    }
  }

  fn push_state(&mut self) {
    self.state = LexerState::Push;
  }

  fn token(&mut self, line: usize) -> (TokenKind, String) {
    let last_layout = self.layout.last();

    let cond = last_layout.is_some() && self.column < *last_layout.unwrap();
    if line != self.line || cond {
      let column = self.column;
      let last = self.layout.last();

      match last {
        None => (),
        Some(last_column) if column > *last_column => (),
        Some(last_column) if column < *last_column => {
          self.pop_layout();
          return (TokenKind::VEnd, String::new());
        }
        Some(_) => return (TokenKind::VSep, String::new()),
      }
    }

    let result = if let Some(char) = self.advance() {
      match char {
        '(' => TokenKind::LParens,
        ')' => TokenKind::RParens,
        '[' => TokenKind::LBracket,
        ']' => TokenKind::RBracket,
        '{' => TokenKind::LBrace,
        '}' => TokenKind::RBrace,
        '#' => {
          self.start = self.index;
          self.advance_while(|c| c.is_ascii_alphanumeric());
          TokenKind::Atom
        }
        '"' => return self.string(),
        ',' => TokenKind::Comma,
        ';' => TokenKind::Semicolon,
        '+' => TokenKind::Plus,
        '-' => {
          if let Some('>') = self.peekable.peek() {
            self.advance();
            self.push_state();
            TokenKind::Arrow
          } else {
            TokenKind::Minus
          }
        }
        '*' => TokenKind::Star,
        '/' => TokenKind::Slash,
        '=' => {
          if let Some('=') = self.peekable.peek() {
            self.advance();
            TokenKind::EqualsEquals
          } else {
            TokenKind::Equals
          }
        }
        '<' => {
          if let Some('>') = self.peekable.peek() {
            self.advance();
            TokenKind::LtGt
          } else {
            TokenKind::Error
          }
        }
        '_' => TokenKind::Wildcard,
        '|' => TokenKind::Pipe,
        c if c.is_ascii_digit() => {
          self.advance_while(|c| c.is_ascii_digit());
          TokenKind::Number
        }
        c if c.is_ascii_alphabetic() => {
          self.advance_while(|c| c.is_ascii_alphanumeric() || *c == '_');
          match &self.input[self.start..self.index] {
            "fun" => TokenKind::Fun,
            "do" => {
              self.push_state();
              TokenKind::Do
            }
            "end" => TokenKind::End,
            "case" => TokenKind::Case,
            "of" => {
              self.push_state();
              TokenKind::Of
            }
            "let" => TokenKind::Let,
            _ => TokenKind::Identifier,
          }
        }
        _ => TokenKind::Error,
      }
    } else if self.layout.pop().is_some() {
      TokenKind::VEnd
    } else {
      TokenKind::Eof
    };
    let lexeme = self.input[self.start..self.index].to_owned();
    (result, lexeme)
  }

  pub fn next_token(&mut self) -> Token {
    let line = self.line;
    self.skip();
    self.start = self.index;

    let (kind, lexeme) = match self.state {
      LexerState::Default => self.token(line),
      LexerState::Push => {
        self.state = LexerState::Default;

        let last = self.layout.last().copied().unwrap_or_default();

        if self.column <= last {
          self.token(line)
        } else {
          self.layout.push(self.column);
          (TokenKind::VBeg, String::new())
        }
      }
    };

    Token { kind, lexeme }
  }

  pub fn pop_layout(&mut self) {
    self.layout.pop();
  }
}

impl<'src> Iterator for Lexer<'src> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index >= self.input.len() {
      None
    } else {
      Some(self.next_token())
    }
  }
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      TokenKind::VBeg => write!(f, "Block Begin"),
      TokenKind::VEnd => write!(f, "Block End"),
      TokenKind::VSep => write!(f, "Separator"),
      TokenKind::LParens => write!(f, "("),
      TokenKind::RParens => write!(f, ")"),
      TokenKind::LBracket => write!(f, "["),
      TokenKind::RBracket => write!(f, "]"),
      TokenKind::LBrace => write!(f, "{{"),
      TokenKind::RBrace => write!(f, "}}"),
      TokenKind::Wildcard => write!(f, "_"),
      TokenKind::Comma => write!(f, ","),
      TokenKind::Semicolon => write!(f, ";"),
      TokenKind::Pipe => write!(f, "|"),
      TokenKind::Plus => write!(f, "+"),
      TokenKind::Minus => write!(f, "-"),
      TokenKind::Star => write!(f, "*"),
      TokenKind::Slash => write!(f, "/"),
      TokenKind::Equals => write!(f, "="),
      TokenKind::EqualsEquals => write!(f, "=="),
      TokenKind::LtGt => write!(f, "<>"),
      TokenKind::Identifier => write!(f, "Identifier"),
      TokenKind::Number => write!(f, "Number"),
      TokenKind::String => write!(f, "String"),
      TokenKind::Atom => write!(f, "Atom"),
      TokenKind::Fun => write!(f, "fun"),
      TokenKind::Do => write!(f, "do"),
      TokenKind::Case => write!(f, "case"),
      TokenKind::Of => write!(f, "of"),
      TokenKind::End => write!(f, "end"),
      TokenKind::Let => write!(f, "let"),
      TokenKind::Arrow => write!(f, "->"),
      TokenKind::Error => write!(f, "Error"),
      TokenKind::Eof => write!(f, "Eof"),
    }
  }
}

#[cfg(test)]
mod test {
  use super::Lexer;

  #[test]
  fn test_lexer() {
    let input = r#"
// comment here
fun main() do
  let tup = {1, 2}
  case tup of
    {a, b} ->
      let res = a + b
      res
    {a} -> 2
  end
end
"#;
    let mut lexer = Lexer::new(input);
    while let Some(token) = lexer.next() {
      println!("{token:?}");
    }
  }
}
