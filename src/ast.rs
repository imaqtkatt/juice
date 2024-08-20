#[derive(Debug)]
pub enum LitExpression {
  Number(i32),
  String(String),
  Atom(String),
  Tuple(Vec<Expression>),
  List(Vec<Expression>),
}

#[derive(Debug)]
pub enum Expression {
  Var(String),
  Literal(LitExpression),
  Assignment(Pattern, Box<Expression>, Box<Expression>),
  Binary(Operation, Box<Expression>, Box<Expression>),
  Call(Box<Expression>, Vec<Expression>),
  Case(Vec<Expression>, Vec<Arm>),
  If(Box<Expression>, Box<Expression>, Box<Expression>),
  Try(Pattern, Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Operation {
  Add,
  Sub,
  Mul,
  Div,
  Eql,
  PtrEq,
}

#[derive(Debug)]
pub struct Arm {
  pub lhs: Vec<Pattern>,
  pub rhs: Expression,
}

#[derive(Debug)]
pub enum LitPattern {
  Number(i32),
  String(String, Option<Box<Pattern>>),
  Atom(String),
  Tuple(Vec<Pattern>),
  List(Vec<Pattern>, Option<Box<Pattern>>),
}

#[derive(Debug, Default)]
pub enum Pattern {
  #[default]
  Wildcard,
  Var(String),
  Literal(LitPattern),
}

#[derive(Debug)]
pub struct FunDefinition {
  pub name: String,
  pub clauses: Vec<FunClause>,
}

#[derive(Debug)]
pub struct FunClause {
  pub patterns: Vec<Pattern>,
  pub body: Expression,
}

#[derive(Debug)]
pub struct Module {
  pub name: String,
  pub fun_definitions: indexmap::IndexMap<String, FunDefinition>,
}

impl FunClause {
  pub fn is_irrefutable(&self) -> bool {
    self
      .patterns
      .iter()
      .all(|p| matches!(p, Pattern::Wildcard | Pattern::Var(..)))
  }
}
