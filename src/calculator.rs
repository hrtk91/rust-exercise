trait Operator {
  fn execute(&self, left: f32, right: f32) -> f32;
}

#[derive(Debug, Clone, Copy)]
pub struct Plus;
impl Operator for Plus {
  fn execute(&self, left: f32, right: f32) -> f32 {
    left + right
  }
}
impl From<Plus> for Operators {
  fn from(val: Plus) -> Self {
    Operators::Plus(val)
  }
}

#[derive(Debug, Clone, Copy)]
 pub struct Minus;
impl Operator for Minus {
  fn execute(&self, left: f32, right: f32) -> f32 {
    left - right
  }
}
impl From<Minus> for Operators {
  fn from(val: Minus) -> Self {
    Operators::Minus(val)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Mul;
impl Operator for Mul {
  fn execute(&self, left: f32, right: f32) -> f32 {
    left * right
  }
}
impl From<Mul> for Operators {
  fn from(val: Mul) -> Self {
    Operators::Mul(val)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Div;
impl Operator for Div {
  fn execute(&self, left: f32, right: f32) -> f32 {
    left / right
  }
}
impl From<Div> for Operators {
  fn from(val: Div) -> Self {
    Operators::Div(val)
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Operators {
  Plus(Plus),
  Minus(Minus),
  Mul(Mul),
  Div(Div)
}

impl Operators {
  pub fn execute(&self, left: f32, right: f32) -> f32 {
    match self {
      Operators::Plus(op) => op.execute(left, right),
      Operators::Minus(op) => op.execute(left, right),
      Operators::Mul(op) => op.execute(left, right),
      Operators::Div(op) => op.execute(left, right)
    }
  }
}

#[derive(Clone)]
pub enum Expression {
  Value(f32),
  Plus(Box<Expression>, Box<Expression>),
  Minus(Box<Expression>, Box<Expression>),
  Mul(Box<Expression>, Box<Expression>),
  Div(Box<Expression>, Box<Expression>),
}

impl Expression {
  pub fn plus(left: f32, right: f32) -> Expression {
    Expression::Plus(Box::new(Expression::Value(left)), Box::new(Expression::Value(right)))
  }
  pub fn minus(left: f32, right: f32) -> Expression {
    Expression::Minus(Box::new(Expression::Value(left)), Box::new(Expression::Value(right)))
  }
  pub fn mul(left: f32, right: f32) -> Expression {
    Expression::Mul(Box::new(Expression::Value(left)), Box::new(Expression::Value(right)))
  }
  pub fn div(left: f32, right: f32) -> Expression {
    Expression::Div(Box::new(Expression::Value(left)), Box::new(Expression::Value(right)))
  }
  pub fn execute(&self) -> f32 {
    match self {
      Expression::Value(value) => *value,
      Expression::Plus(left, right) => Plus.execute(left.execute(), right.execute()),
      Expression::Minus(left, right) => Minus.execute(left.execute(), right.execute()),
      Expression::Mul(left, right) => Mul.execute(left.execute(), right.execute()),
      Expression::Div(left, right) => Div.execute(left.execute(), right.execute()),
    }
  }
}

impl Into<Expression> for f32 {
  fn into(self) -> Expression {
    Expression::Value(self)
  }
}

pub struct Calculator {
  expr: Option<Expression>
}

impl Default for Calculator {
    fn default() -> Self {
        Self::new()
    }
}

impl Calculator {
  pub fn new() -> Self {
    Calculator {
      expr: None
    }
  }

  pub fn term(mut self, value: f32) -> Self {
    self.expr = Some(Expression::Value(value));
    self
  }

  pub fn plus(mut self, value: f32) -> Self {
    if let Some(expr) = self.expr.take() {
      self.expr = Some(Expression::Plus(Box::new(expr), Box::new(Expression::Value(value))));
    } else {
      self.expr = Some(Expression::Value(value));
    }
    self
  }

  pub fn minus(mut self, value: f32) -> Self {
    if let Some(expr) = self.expr.take() {
      self.expr = Some(Expression::Minus(Box::new(expr), Box::new(Expression::Value(value))));
    } else {
      self.expr = Some(Expression::Value(value));
    }
    self
  }

  pub fn mul(mut self, value: f32) -> Self {
    if let Some(expr) = self.expr.take() {
      self.expr = Some(Expression::Mul(Box::new(expr), Box::new(Expression::Value(value))));
    } else {
      self.expr = Some(Expression::Value(value));
    }
    self
  }

  pub fn div(mut self, value: f32) -> Self {
    if let Some(expr) = self.expr.take() {
      self.expr = Some(Expression::Div(Box::new(expr), Box::new(Expression::Value(value))));
    } else {
      self.expr = Some(Expression::Value(value));
    }
    self
  }

  fn match_expression(op: impl Into<Operators>, left: impl Into<Expression>, right: impl Into<Expression>) -> Expression {
    match op.into() {
      Operators::Plus(_) => Expression::Plus(Box::new(left.into()), Box::new(right.into())),
      Operators::Minus(_) => Expression::Minus(Box::new(left.into()), Box::new(right.into())),
      Operators::Mul(_) => Expression::Mul(Box::new(left.into()), Box::new(right.into())),
      Operators::Div(_) => Expression::Div(Box::new(left.into()), Box::new(right.into())),
    }
  }

  pub fn lhs_precedence(mut self, left: impl Into<Expression>, op: impl Into<Operators>) -> Self {
    if let Some(expr) = self.expr.take() {
      self.expr = match expr {
        Expression::Plus(lhs, rhs) => Some(Expression::Plus(Box::new(Calculator::match_expression(op, left.into(), *lhs)), rhs)),
        Expression::Minus(lhs, rhs) => Some(Expression::Minus(Box::new(Calculator::match_expression(op, left.into(), *lhs)), rhs)),
        Expression::Mul(lhs, rhs) => Some(Calculator::match_expression(op, left.into(), Expression::Mul(lhs, rhs))),
        Expression::Div(lhs, rhs) => Some(Calculator::match_expression(op, left.into(), Expression::Div(lhs, rhs))),
        Expression::Value(value) => Some(Calculator::match_expression(op, left.into(), Expression::Value(value))),
      };
    } else {
      self.expr = Some(left.into());
    }
    self
  }

  pub fn rhs_precedence(mut self,  op: impl Into<Operators>, right: impl Into<Expression>) -> Self {
    if let Some(expr) = self.expr.take() {
      self.expr = match expr {
        Expression::Plus(lhs, rhs) => Some(Expression::Plus(lhs, Box::new(Calculator::match_expression(op, right.into(), *rhs)))),
        Expression::Minus(lhs, rhs) => Some(Expression::Minus(lhs, Box::new(Calculator::match_expression(op, right.into(), *rhs)))),
        Expression::Mul(lhs, rhs) => Some(Calculator::match_expression(op, right.into(), Expression::Mul(lhs, rhs))),
        Expression::Div(lhs, rhs) => Some(Calculator::match_expression(op, right.into(), Expression::Div(lhs, rhs))),
        Expression::Value(value) => Some(Calculator::match_expression(op, right.into(), Expression::Value(value))),
      };
    } else {
      self.expr = Some(right.into());
    }
    self
  }

  pub fn result(&mut self) -> f32 {
    self.expr.take().unwrap_or(0.0.into()).execute()
  }
}

impl Into<Expression> for Calculator {
  fn into(self) -> Expression {
    self.expr.unwrap_or(0.0.into())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {

    // (3 + 2) * 1 + 1
    let mut calculator = Calculator::new()
      .term(1.0)
      .plus(1.0)
      .lhs_precedence(
        Calculator::new()
          .term(3.0)
          .plus(2.0),
          Mul
      );
    let result = calculator.result();

    assert_eq!(result, 6.0);

    assert_eq!(calculator.plus(2.0).mul(3.0).div(2.0).result(), 3.0);

    let result = Expression::Plus(
      Box::new(Expression::plus(4.0, 3.0)),
      Box::new(Expression::div(1.0, 5.0))
    ).execute();

    assert_eq!(result, 7.2);
  }
}