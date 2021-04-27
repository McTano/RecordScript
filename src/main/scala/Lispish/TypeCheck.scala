import scala.util.{Success, Try, Failure}

object TypeCheck {
  type CheckerResult = (Type, Context[Type])
  def apply(program: Expression): Try[CheckerResult] = {
    TypeCheck(program, NullContext)
  }

  def apply(op: Operator): Fun = {
    op.signature
  }
  def apply(
      expression: Expression,
      context: Context[Type]
  ): Try[CheckerResult] = {
    expression match {
      case LetExpr(binders, targetExpression) =>
        Try(
          binders.foldLeft[(Type, Context[Type])]((Top, context))(
            (prev: (Type, Context[Type]), current: (Var, Expression)) => {
              current match {
                case (variable, boundExpr) => {
                  TypeCheck(boundExpr, prev._2).get match {
                    case (t, c) => (t, Binding(c, variable, t))
                  }
                }
              }
            }
          )
        )
      case Binop(op, e1, e2) =>
        TypeCheck(e1).map(res =>
          res match {
            case (t1, _) =>
              TypeCheck(e2).get match {
                case (t2, _) =>
                  TypeCheck(op) match {
                    case Fun(lhsType, rhsType, resType) =>
                      if (lhsType == t1) {
                        if (rhsType == t2) {
                          (resType, context)
                        } else {
                          throw new OperatorTypeMismatch(op, rhsType, t2)
                        }
                      } else {
                        throw new OperatorTypeMismatch(op, lhsType, t1)
                      }
                  }
              }
          }
        )
      case Num(n) => Success(NumType, context)
      case (v: Var) =>
        context.lookup(v) match {
          case Some(t) => Success(t, context)
          case None    => throw new UnboundVarLookup(v, context)
        }
    }
  }
}
