use crate::{
  ast,
  desugar::{pattern, Expression, Pattern},
};

use super::{Desugar, FunDefinition};

impl Desugar for ast::FunDefinition {
  type Out = FunDefinition;

  type Err = String;

  fn desugar(self) -> Result<Self::Out, Self::Err> {
    fn gen_name(name: usize) -> String {
      format!("x{name}")
    }

    assert!(self.clauses.len() > 0);

    let mut patterns: Vec<Vec<_>> = vec![];
    let mut actions = vec![];
    for clause in self.clauses {
      patterns.push(
        clause
          .patterns
          .into_iter()
          .map(|p| p.desugar())
          .collect::<Result<_, _>>()?,
      );
      actions.push(
        clause
          .body
          .desugar()
          .map_err(|_| format!("Could not desugar clause body"))?,
      );
    }

    let arity = (&patterns[0]).len();
    let parameters: Vec<String> = (0..arity).map(gen_name).collect();
    if patterns.len() == 1 && patterns[0].iter().all(Pattern::is_irrefutable) {
      let body = actions.into_iter().nth(0).unwrap();

      Ok(FunDefinition {
        name: self.name,
        parameters,
        body,
      })
    } else {
      for pat in patterns.iter() {
        let curr_arity = pat.len();
        if curr_arity != arity {
          return Err(format!(
            "{}: Arity error, expected {arity} but got {curr_arity}",
            self.name
          ));
        }
      }

      let (tree, actions) =
        pattern::Problem::with_parameters(parameters.clone(), patterns, actions);

      Ok(FunDefinition {
        name: self.name,
        parameters,
        body: Expression::Case(tree, actions),
      })
    }
  }
}
