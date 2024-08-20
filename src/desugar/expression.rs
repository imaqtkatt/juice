use crate::ast::{self, Arm};

use super::{pattern, Desugar, Expression, LitExpression, Operation, Pattern};

impl Desugar for ast::Expression {
  type Out = Expression;

  type Err = String;

  fn desugar(self) -> Result<Self::Out, Self::Err> {
    match self {
      ast::Expression::Var(name) => Ok(Expression::Var(name)),
      ast::Expression::Literal(lit) => Ok(Expression::Literal(match lit {
        ast::LitExpression::Number(n) => LitExpression::Number(n),
        ast::LitExpression::String(s) => LitExpression::List(
          s.chars()
            .map(|c| Expression::Literal(LitExpression::Number(u32::from(c) as i32)))
            .collect(),
        ),
        ast::LitExpression::Atom(a) => LitExpression::Atom(a),
        ast::LitExpression::Tuple(t) => {
          LitExpression::Tuple(t.into_iter().map(Self::desugar).collect::<Result<_, _>>()?)
        }
        ast::LitExpression::List(l) => {
          LitExpression::List(l.into_iter().map(Self::desugar).collect::<Result<_, _>>()?)
        }
      })),
      ast::Expression::Assignment(
        pat @ ast::Pattern::Wildcard | pat @ ast::Pattern::Var(..),
        value,
        next,
      ) => {
        let name = if let ast::Pattern::Var(name) = pat {
          Some(name)
        } else {
          None
        };
        let value = value.desugar()?;
        let next = next.desugar()?;
        Ok(Expression::LetIn(name, Box::new(value), Box::new(next)))
      }
      ast::Expression::Assignment(pat, value, next) => {
        let scrutinee = vec![value.desugar()?];
        let pat = vec![pat.desugar()?];
        let action = vec![next.desugar()?];
        Ok(pattern::Problem::compile(scrutinee, vec![pat], action))
      }
      ast::Expression::Binary(op, lhs, rhs) => {
        let op = match op {
          ast::Operation::Add => Operation::Add,
          ast::Operation::Sub => Operation::Sub,
          ast::Operation::Mul => Operation::Mul,
          ast::Operation::Div => Operation::Div,
          ast::Operation::Eql => Operation::Eql,
          ast::Operation::PtrEq => Operation::PtrEq,
        };
        let lhs = lhs.desugar()?;
        let rhs = rhs.desugar()?;
        Ok(Expression::Binary(op, Box::new(lhs), Box::new(rhs)))
      }
      ast::Expression::Call(name, arguments) => match name.desugar()? {
        Expression::Var(name) => Ok(Expression::Call(
          name,
          arguments
            .into_iter()
            .map(Self::desugar)
            .collect::<Result<_, _>>()?,
        )),
        _ => Err("Callee is not a valid expression".to_string()),
      },
      ast::Expression::Case(scrutinees, arms) => {
        let matching = scrutinees.len();
        let mut left = vec![];
        let mut right = vec![];
        for Arm { lhs, rhs } in arms.into_iter() {
          let patterns_len = lhs.len();
          if patterns_len != matching {
            return Err(format!(
              "Mismatched number of patterns: expected {}, found {}",
              matching, patterns_len,
            ));
          }
          left.push(
            lhs
              .into_iter()
              .map(|p| p.desugar())
              .collect::<Result<_, _>>()?,
          );
          right.push(rhs.desugar()?);
        }
        let scrutinee = scrutinees.into_iter().flat_map(|s| s.desugar()).collect();
        Ok(pattern::Problem::compile(scrutinee, left, right))
      }
      ast::Expression::If(cond, then, r#else) => {
        let cond = cond.desugar()?;
        let then = then.desugar()?;
        let r#else = r#else.desugar()?;
        Ok(Expression::If(
          Box::new(cond),
          Box::new(then),
          Box::new(r#else),
        ))
      }
      ast::Expression::Try(pat, value, then, r#else) => {
        let scrutinee = vec![value.desugar()?];
        let patterns = vec![vec![pat.desugar()?], vec![Pattern::Wildcard]];
        let actions = vec![then.desugar()?, r#else.desugar()?];
        Ok(pattern::Problem::compile(scrutinee, patterns, actions))
      }
    }
  }
}
