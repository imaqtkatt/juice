use std::{
  collections::{BTreeSet, VecDeque},
  rc::Rc,
};

use crate::{
  ast,
  case_tree::{Access, Case, CaseTree, Occurrence},
};

use super::{Desugar, Expression, LitPattern, Pattern};

impl Desugar for ast::Pattern {
  type Out = Pattern;

  type Err = String;

  fn desugar(self) -> Result<Self::Out, Self::Err> {
    match self {
      ast::Pattern::Wildcard => Ok(Pattern::Wildcard),
      ast::Pattern::Var(name) => Ok(Pattern::Var(name)),
      ast::Pattern::Literal(l) => Ok(Pattern::Literal(match l {
        ast::LitPattern::Number(n) => LitPattern::Number(n),
        ast::LitPattern::String(s, tail) => {
          let tail = if let Some(tail) = tail {
            tail.desugar()?
          } else {
            Pattern::Literal(LitPattern::Nil)
          };
          return Ok(s.chars().into_iter().rfold(tail, |acc, next| {
            let next = Pattern::Literal(LitPattern::Number(u32::from(next) as i32));
            Pattern::Literal(LitPattern::Cons(Box::new(next), Box::new(acc)))
          }));
        }
        ast::LitPattern::Atom(a) => LitPattern::Atom(a),
        ast::LitPattern::Tuple(t) => {
          LitPattern::Tuple(t.into_iter().map(Self::desugar).collect::<Result<_, _>>()?)
        }
        ast::LitPattern::List(els, tail) => {
          let tail = if let Some(tail) = tail {
            tail.desugar()?
          } else {
            Pattern::Literal(LitPattern::Nil)
          };
          let els = els
            .into_iter()
            .map(Self::desugar)
            .collect::<Result<Vec<_>, _>>()?;
          return Ok(els.into_iter().rfold(tail, |acc, next| {
            Pattern::Literal(LitPattern::Cons(Box::new(next), Box::new(acc)))
          }));
        }
      })),
    }
  }
}

impl Pattern {
  pub fn binders(&self, e: Expression) -> Vec<(String, Occurrence)> {
    fn bind(pat: &Pattern, occ: Occurrence, binders: &mut Vec<(String, Occurrence)>) {
      match pat {
        Pattern::Wildcard => {}
        Pattern::Var(name) => binders.push((name.clone(), occ)),
        Pattern::Literal(lit) => match lit {
          LitPattern::Number(_) => {}
          LitPattern::Atom(_) => {}
          LitPattern::Tuple(elements) => {
            for (i, e) in elements.iter().enumerate() {
              bind(e, occ.with_access(Access::Tuple(i)), binders);
            }
          }
          LitPattern::Cons(head, tail) => {
            bind(head, occ.with_access(Access::Head), binders);
            bind(tail, occ.with_access(Access::Tail), binders);
          }
          LitPattern::Nil => {}
        },
      }
    }
    let mut binders = vec![];
    bind(self, Occurrence(e, vec![]), &mut binders);
    binders
  }
}

#[derive(Clone, Debug)]
pub struct Match {
  occurrence: Occurrence,
  pattern: Pattern,
}

#[derive(Clone, Debug)]
pub struct Row(pub VecDeque<Match>, pub usize);

type Matrix = Vec<Row>;

impl Match {
  fn derive(occurrence: &Occurrence, pattern: Pattern, acc: Access) -> Self {
    Self {
      occurrence: occurrence.with_access(acc),
      pattern,
    }
  }

  fn wildcard(occurrence: &Occurrence) -> Self {
    Self {
      occurrence: occurrence.clone(),
      pattern: Pattern::Wildcard,
    }
  }

  pub fn specialize(self, case: Case) -> Option<VecDeque<Match>> {
    match (self.pattern, case) {
      (Pattern::Var(..) | Pattern::Wildcard, Case::Number(..)) => Some(VecDeque::new()),
      (Pattern::Var(..) | Pattern::Wildcard, Case::Atom(..)) => Some(VecDeque::new()),
      (Pattern::Var(..) | Pattern::Wildcard, Case::Tuple(size)) => Some(
        (0..size)
          .map(|_| Match::wildcard(&self.occurrence))
          .collect(),
      ),
      (Pattern::Var(..) | Pattern::Wildcard, Case::Cons) => {
        Some((0..2).map(|_| Match::wildcard(&self.occurrence)).collect())
      }
      (Pattern::Var(..) | Pattern::Wildcard, Case::Nil) => Some(VecDeque::new()),

      (Pattern::Literal(LitPattern::Number(a)), Case::Number(b)) if a == b => Some(VecDeque::new()),
      (Pattern::Literal(LitPattern::Atom(a)), Case::Atom(b)) if a == b => Some(VecDeque::new()),
      (Pattern::Literal(LitPattern::Tuple(a)), Case::Tuple(b)) if a.len() == b => Some(
        a.into_iter()
          .enumerate()
          .map(|(idx, pat)| Self::derive(&self.occurrence, pat, Access::Tuple(idx)))
          .collect(),
      ),
      (Pattern::Literal(LitPattern::Cons(head, tail)), Case::Cons) => {
        let mut deque = VecDeque::new();
        deque.push_back(Self::derive(&self.occurrence, *head, Access::Head));
        deque.push_back(Self::derive(&self.occurrence, *tail, Access::Tail));
        Some(deque)
      }
      (Pattern::Literal(LitPattern::Nil), Case::Nil) => Some(VecDeque::new()),

      _ => None,
    }
  }

  pub fn default(self) -> Option<VecDeque<Match>> {
    match self.pattern {
      Pattern::Var(..) | Pattern::Wildcard => Some(VecDeque::new()),
      _ => None,
    }
  }

  pub fn as_case(&self) -> Option<Case> {
    match &self.pattern {
      Pattern::Wildcard | Pattern::Var(..) => None,
      Pattern::Literal(lit) => Some(match lit {
        LitPattern::Number(n) => Case::Number(*n),
        LitPattern::Atom(a) => Case::Atom(a.clone()),
        LitPattern::Tuple(pats) => Case::Tuple(pats.len()),
        LitPattern::Cons(_, _) => Case::Cons,
        LitPattern::Nil => Case::Nil,
      }),
    }
  }
}

impl Row {
  pub fn default(mut self) -> Option<Self> {
    let head = self.0.pop_front()?;
    let mut tail = self.0;
    let mut new_head = head.default()?;
    new_head.append(&mut tail);

    Some(Row(new_head, self.1))
  }

  pub fn specialize(mut self, case: Case) -> Option<Self> {
    let head = self.0.pop_front()?;
    let mut tail = self.0;
    let mut new_head = head.specialize(case)?;
    new_head.append(&mut tail);

    Some(Row(new_head, self.1))
  }

  pub fn head_case(&self) -> Option<Case> {
    self.0.front()?.as_case()
  }
}

pub struct Problem {
  matrix: Matrix,
}

impl Problem {
  pub fn new(patterns: Vec<Vec<Pattern>>, scrutinee: Vec<Expression>) -> Self {
    let matrix = patterns
      .into_iter()
      .enumerate()
      .map(|(idx, patterns)| {
        let mut row = VecDeque::new();
        for (idx, pattern) in patterns.into_iter().enumerate() {
          row.push_back(Match {
            occurrence: Occurrence(scrutinee[idx].clone(), vec![]),
            pattern,
          })
        }
        Row(row, idx)
      })
      .collect();
    Problem { matrix }
  }

  pub fn matching_leaf(&self) -> Option<usize> {
    let head = self.matrix.first()?;

    if head.0.is_empty() {
      Some(head.1)
    } else {
      None
    }
  }

  pub fn head_occurrence(&self) -> Occurrence {
    self.matrix[0].0[0].occurrence.clone()
  }

  pub fn head_cases(&self) -> BTreeSet<Case> {
    let mut cases = BTreeSet::new();

    for row in &self.matrix {
      if let Some(case) = row.head_case() {
        cases.insert(case);
      }
    }

    cases
  }

  pub fn default(self) -> CaseTree {
    let matrix = self
      .matrix
      .into_iter()
      .filter_map(|row| row.default())
      .collect();
    Problem { matrix }.derive()
  }

  pub fn specialize(&self, case: Case) -> CaseTree {
    let matrix = self
      .matrix
      .iter()
      .filter_map(|row| row.clone().specialize(case.clone()))
      .collect();
    Problem { matrix }.derive()
  }

  pub fn derive(self) -> CaseTree {
    if self.matrix.is_empty() {
      CaseTree::Fail
    } else if let Some(leaf) = self.matching_leaf() {
      CaseTree::Leaf(leaf)
    } else {
      let occurrence = self.head_occurrence();
      let mut branches = vec![];
      let cases = self.head_cases();

      for case in cases {
        branches.push((case.clone(), self.specialize(case)));
      }

      let default = Box::new(self.default());

      if branches.is_empty() {
        *default
      } else {
        CaseTree::Switch(Rc::new(occurrence), branches, default)
      }
    }
  }
}

impl Problem {
  pub fn compile(
    scrutinee: Vec<Expression>,
    patterns: Vec<Vec<Pattern>>,
    actions: Vec<Expression>,
  ) -> Expression {
    fn gen_scrutinee_name(e: &Expression, gen: &mut usize) -> (String, bool) {
      *gen += 1;
      match e {
        Expression::Var(name) => (name.clone(), false),
        _ => (format!("a_{gen}"), true),
      }
    }

    let mut gen = 0;
    let (names, need_let): (Vec<_>, Vec<_>) = scrutinee
      .iter()
      .map(|e| gen_scrutinee_name(e, &mut gen))
      .unzip();
    let (tree, actions) = Problem::with_parameters(names.clone(), patterns, actions);
    names.iter().zip(need_let).zip(scrutinee).fold(
      Expression::Case(tree, actions),
      |x, ((name, need_let), scrutinee)| {
        if need_let {
          Expression::LetIn(Some(name.clone()), Box::new(scrutinee), Box::new(x))
        } else {
          x
        }
      },
    )
  }

  pub fn with_parameters(
    parameters: Vec<String>,
    patterns: Vec<Vec<Pattern>>,
    actions: Vec<Expression>,
  ) -> (CaseTree, Vec<Expression>) {
    let mut new_actions = vec![];

    for (pats, mut action) in patterns.iter().zip(actions) {
      let mut ctx = vec![];
      for (scrutinee, pat) in parameters.iter().zip(pats) {
        let e = Expression::Var(scrutinee.clone());
        for (binder, name) in pat.binders(e) {
          ctx.push((binder, name.to_expression()));
        }
      }
      action = ctx.into_iter().fold(action, |acc, (binder, name)| {
        Expression::LetIn(Some(binder), name.into(), acc.into())
      });
      new_actions.push(action);
    }

    let scrutinee = parameters
      .iter()
      .map(|name| Expression::Var(name.clone()))
      .collect();
    let tree = Problem::new(patterns, scrutinee).derive();
    (tree, new_actions)
  }
}
