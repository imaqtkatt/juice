use std::collections::HashMap;

use crate::{
  case_tree::{CaseTree, Occurrence},
  desugar::{Expression, FunDefinition, LitExpression, Module},
  report::{self, Reporter},
};

struct UnboundVariable(String, Option<String>);

impl report::Diagnostic for UnboundVariable {
  fn severity(&self) -> report::Severity {
    report::Severity::Error
  }

  fn message(&self) -> String {
    format!("Unbound variable '{}'.", self.0)
  }

  fn hint(&self) -> Option<String> {
    if let Some(name) = &self.1 {
      Some(format!(
        "A similar variable '{name}' is available in scope."
      ))
    } else {
      None
    }
  }
}

struct Context<'a> {
  scope: Scope<'a>,
  reporter: &'a Reporter,
}

impl<'a> Context<'a> {
  fn new(reporter: &'a Reporter, scope: HashMap<&'a String, usize>) -> Self {
    Self { scope, reporter }
  }

  fn push(&mut self, name: &'a String) {
    *self.scope.entry(name).or_default() += 1;
  }

  fn pop(&mut self, name: &'a String) {
    match self.scope.entry(name) {
      std::collections::hash_map::Entry::Occupied(mut o) => {
        *o.get_mut() -= 1;
        if *o.get() == 0 {
          o.remove();
        }
      }
      std::collections::hash_map::Entry::Vacant(_) => unreachable!(),
    }
  }

  fn report(&self, diagnostic: UnboundVariable) {
    self.reporter.report(diagnostic);
  }
}

type Scope<'a> = HashMap<&'a String, usize>;

impl Expression {
  fn check_unbound_variables<'a>(&'a self, context: &mut Context<'a>) {
    match self {
      Expression::Var(name) => {
        if context.scope.contains_key(name) {
        } else {
          let most_similar = find_most_similar_string(context.scope.keys().cloned(), name);
          context.report(UnboundVariable(name.clone(), most_similar.cloned()));
        }
      }
      Expression::Literal(lit) => match lit {
        LitExpression::Tuple(exprs) | LitExpression::List(exprs) => {
          for e in exprs {
            e.check_unbound_variables(context);
          }
        }
        LitExpression::Number(_) | LitExpression::Atom(_) => (),
      },
      Expression::LetIn(name, value, next) => {
        value.check_unbound_variables(context);
        if let Some(name) = name {
          context.push(name);
        }
        next.check_unbound_variables(context);
        if let Some(name) = name {
          context.pop(name);
        }
      }
      Expression::Binary(_, lhs, rhs) => {
        lhs.check_unbound_variables(context);
        rhs.check_unbound_variables(context);
      }
      Expression::Call(_, arguments) => {
        for a in arguments {
          a.check_unbound_variables(context);
        }
      }
      Expression::Case(case_tree, actions) => {
        case_tree.check_unbound_variables(context);
        for a in actions {
          a.check_unbound_variables(context);
        }
      }
      Expression::If(cond, then, r#else) => {
        cond.check_unbound_variables(context);
        then.check_unbound_variables(context);
        r#else.check_unbound_variables(context);
      }
      Expression::Access(e, _) => e.check_unbound_variables(context),
    }
  }
}

impl CaseTree {
  fn check_unbound_variables<'a>(&'a self, context: &mut Context<'a>) {
    match self {
      CaseTree::Fail => (),
      CaseTree::Leaf(_) => (),
      CaseTree::Switch(occurrence, branches, default) => {
        occurrence.check_unbound_variables(context);
        for (_, branch) in branches {
          branch.check_unbound_variables(context);
        }
        default.check_unbound_variables(context);
      }
    }
  }
}

impl Occurrence {
  fn check_unbound_variables<'a>(&'a self, context: &mut Context<'a>) {
    self.0.check_unbound_variables(context);
  }
}

impl FunDefinition {
  pub fn check_unbound_variables<'a>(&'a self, reporter: &Reporter) {
    let mut scope = HashMap::new();
    for p in self.parameters.iter() {
      scope.insert(p, 1);
    }
    let mut context = Context::new(reporter, scope);
    self.body.check_unbound_variables(&mut context);
  }
}

impl Module {
  pub fn check_unbound_variables<'a>(&'a self, repoter: &Reporter) {
    for fun in self.fun_definitions.values() {
      fun.check_unbound_variables(repoter);
    }
  }
}

fn find_most_similar_string<'a>(
  set: impl Iterator<Item = &'a String>,
  target: &String,
) -> Option<&'a String> {
  let mut last = 0.;
  let mut most_similar = None;
  for s in set {
    let sim = jaro(target, s);
    if sim > 0.7 && sim > last {
      most_similar = Some(s);
    } else {
      last = sim;
    }
  }
  most_similar
}

fn jaro(s1: &str, s2: &str) -> f64 {
  let len1 = s1.chars().count();
  let len2 = s2.chars().count();

  if len1 == 0 || len2 == 0 {
    return 0.0;
  }

  let match_distance = len1.max(len2) / 2 - 1;

  let mut s1_matches = vec![false; len1];
  let mut s2_matches = vec![false; len2];

  let mut matches = 0;
  let mut transpositions = 0;

  for (i, c1) in s1.chars().enumerate() {
    let start = i.saturating_sub(match_distance);
    let end = (i + match_distance + 1).min(len2);

    if start >= end {
      continue;
    }

    for j in start..end {
      let c2 = s2.chars().nth(j).unwrap();
      if s2_matches[j] || c1 != c2 {
        continue;
      }

      s1_matches[i] = true;
      s2_matches[j] = true;
      matches += 1;
      break;
    }
  }

  if matches == 0 {
    return 0.0;
  }

  let mut k = 0;
  for (i, c1) in s1.chars().enumerate() {
    if !s1_matches[i] {
      continue;
    }

    while !s2_matches[k] {
      k += 1;
    }

    if c1 != s2.chars().nth(k).unwrap() {
      transpositions += 1;
    }

    k += 1;
  }

  let matches = matches as f64;
  let transpositions = transpositions as f64 / 2.0;

  ((matches / len1 as f64) + (matches / len2 as f64) + ((matches - transpositions) / matches)) / 3.0
}
