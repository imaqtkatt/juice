pub mod expression;
pub mod function;
pub mod module;
pub mod pattern;

use crate::case_tree;

#[derive(Clone, Debug)]
pub enum LitExpression {
  Number(i32),
  Atom(String),
  Tuple(Vec<Expression>),
  List(Vec<Expression>),
}

#[derive(Clone, Debug)]
pub enum Expression {
  Var(String),
  Literal(LitExpression),
  LetIn(Option<String>, Box<Expression>, Box<Expression>),
  Binary(Operation, Box<Expression>, Box<Expression>),
  Call(String, Vec<Expression>),
  Case(case_tree::CaseTree, Vec<Expression>),
  If(Box<Expression>, Box<Expression>, Box<Expression>),
  Access(Box<Expression>, case_tree::Access),
}

#[derive(Clone, Copy, Debug)]
pub enum Operation {
  Add,
  Sub,
  Mul,
  Div,
  Eql,
  PtrEq,
}

#[derive(Clone, Debug)]
pub enum LitPattern {
  Number(i32),
  Atom(String),
  Tuple(Vec<Pattern>),
  Cons(Box<Pattern>, Box<Pattern>),
  Nil,
}

#[derive(Clone, Debug, Default)]
pub enum Pattern {
  #[default]
  Wildcard,
  Var(String),
  Literal(LitPattern),
}

#[derive(Debug)]
pub struct FunDefinition {
  pub name: String,
  pub parameters: Vec<String>,
  pub body: Expression,
}

#[derive(Debug)]
pub struct Module {
  pub name: String,
  pub fun_definitions: indexmap::IndexMap<String, FunDefinition>,
}

impl Pattern {
  pub fn is_irrefutable(&self) -> bool {
    matches!(self, Pattern::Wildcard | Pattern::Var(..))
  }
}

pub trait Desugar {
  type Out;
  type Err;

  fn desugar(self) -> Result<Self::Out, Self::Err>;
}
