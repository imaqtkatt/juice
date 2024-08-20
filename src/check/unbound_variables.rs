use std::collections::HashMap;

use crate::{
  case_tree::{CaseTree, Occurrence},
  desugar::{Expression, FunDefinition, LitExpression, Module},
  report::{self, Reporter},
};

struct UnboundVariable(String);

impl report::Diagnostic for UnboundVariable {
  fn severity(&self) -> report::Severity {
    report::Severity::Error
  }

  fn message(&self) -> String {
    format!("Unbound variable '{}'.", self.0)
  }
}

struct Context<'a> {
  scope: Scope<'a>,
  reporter: &'a Reporter,
}

impl<'a> Context<'a> {
  fn new(reporter: &'a Reporter) -> Self {
    Self {
      scope: HashMap::new(),
      reporter,
    }
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
}

type Scope<'a> = HashMap<&'a String, usize>;

impl Expression {
  fn check_unbound_variables<'a>(&'a self, context: &mut Context<'a>) {
    match self {
      Expression::Var(name) => {
        if context.scope.contains_key(name) {
        } else {
          context.reporter.report(UnboundVariable(name.clone()));
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
    let mut context = Context::new(reporter);
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
