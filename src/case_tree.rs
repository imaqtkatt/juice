use std::rc;

use crate::desugar::Expression;

#[derive(Clone, Debug)]
pub enum CaseTree {
  Fail,
  Leaf(usize),
  Switch(rc::Rc<Occurrence>, Vec<(Case, CaseTree)>, Box<CaseTree>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Case {
  Number(i32),
  Atom(String),
  Tuple(usize),
  Cons,
  Nil,
}

#[derive(Clone, Debug)]
pub struct Occurrence(pub Expression, pub Vec<Access>);

impl Occurrence {
  pub fn with_access(&self, access: Access) -> Self {
    let mut axs = self.1.clone();
    axs.push(access);
    Self(self.0.clone(), axs)
  }

  pub fn to_expression(self) -> Expression {
    self
      .1
      .into_iter()
      .fold(self.0, |acc, next| Expression::Access(Box::new(acc), next))
  }
}

#[derive(Clone, Debug)]
pub enum Access {
  Tuple(usize),
  Head,
  Tail,
}
