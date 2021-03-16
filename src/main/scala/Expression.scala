// VALUE := true | false
// EXP :=  VALUE | if EXP then EXP else

sealed trait RSExpression {
  def eval(): RSValue
}

case class If(
    condExpr: RSExpression,
    thenExpr: RSExpression,
    elseExpr: RSExpression
) extends RSExpression {

  override def toString(): String = {
    s"if $condExpr then $thenExpr else $elseExpr"
  }

  def eval(): RSValue = {
    condExpr.eval() match {
      case True  => thenExpr.eval()
      case False => elseExpr.eval()
    }
  }
}

sealed trait RSValue extends RSExpression {
  def eval(): RSValue = {
    this
  }
}

sealed trait Bool extends RSValue

case object True extends Bool
case object False extends Bool
