use crate::parser::{BinaryOp, BinaryOperator, Expr, Literal, UnaryOp, UnaryOperator};

/// replace `const=const` with boolean const
#[inline]
pub fn simplified_eq<'a>(left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
    if let Expr::Literal(left_lit) = &left {
        if let Expr::Literal(right_lit) = &right {
            return Literal::Boolean(left_lit == right_lit).into();
        }
    }
    BinaryOp::new(BinaryOperator::Eq, Box::new(left), Box::new(right)).into()
}

/// replace `const!=const` with boolean const
#[inline]
pub fn simplified_neq<'a>(left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
    if let Expr::Literal(left_lit) = &left {
        if let Expr::Literal(right_lit) = &right {
            return Literal::Boolean(left_lit != right_lit).into();
        }
    }
    BinaryOp::new(BinaryOperator::NotEq, Box::new(left), Box::new(right)).into()
}

/// replace `x AND y` if x or y is boolean const
#[inline]
pub fn simplified_and<'a>(left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
    if let Expr::Literal(Literal::Boolean(b)) = left {
        return if b {
            right
        } else {
            Literal::Boolean(false).into()
        };
    }
    if let Expr::Literal(Literal::Boolean(b)) = right {
        return if b {
            left
        } else {
            Literal::Boolean(false).into()
        };
    }
    BinaryOp::new(BinaryOperator::And, Box::new(left), Box::new(right)).into()
}

/// replace `x OR y` if x or y is boolean const
#[inline]
pub fn simplified_or<'a>(left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
    if let Expr::Literal(Literal::Boolean(b)) = left {
        return if b {
            Literal::Boolean(true).into()
        } else {
            right
        };
    }
    if let Expr::Literal(Literal::Boolean(b)) = right {
        return if b {
            Literal::Boolean(true).into()
        } else {
            left
        };
    }
    BinaryOp::new(BinaryOperator::Or, Box::new(left), Box::new(right)).into()
}

/// replace `x XOR y` if x or y is boolean const
#[inline]
pub fn simplified_xor<'a>(left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
    if let Expr::Literal(Literal::Boolean(b)) = left {
        return if b {
            UnaryOp::new(UnaryOperator::Not, Box::new(right)).into()
        } else {
            right
        };
    }
    if let Expr::Literal(Literal::Boolean(b)) = right {
        return if b {
            UnaryOp::new(UnaryOperator::Not, Box::new(left)).into()
        } else {
            left
        };
    }
    BinaryOp::new(BinaryOperator::Xor, Box::new(left), Box::new(right)).into()
}

/// replace `not boolean` with boolean const
#[inline]
pub fn simplified_not(operand: Expr) -> Expr {
    if let Expr::Literal(Literal::Boolean(b)) = operand {
        return Literal::Boolean(!b).into();
    }
    UnaryOp::new(UnaryOperator::Not, Box::new(operand)).into()
}

/// replace `const is null` with boolean const
#[inline]
pub fn simplified_is_null(operand: Expr) -> Expr {
    if let Expr::Literal(lit) = operand {
        return Literal::Boolean(lit == Literal::Null).into();
    }
    UnaryOp::new(UnaryOperator::IsNull, Box::new(operand)).into()
}

// replace `not boolean` with boolean const
#[inline]
pub fn simplified_is_not_null(operand: Expr) -> Expr {
    if let Expr::Literal(lit) = operand {
        return Literal::Boolean(lit != Literal::Null).into();
    }
    UnaryOp::new(UnaryOperator::IsNotNull, Box::new(operand)).into()
}
