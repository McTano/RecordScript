sealed trait Expression
case class LetExpr(
    binders: List[(Var, Expression)],
    targetExpression: Expression
) extends Expression
case class BinopExpr(o: Operator, e1: Expression, e2: Expression)
    extends Expression

sealed trait Token

sealed trait SimpleValue extends Token with Expression
case class NumLiteral(n: Int) extends SimpleValue
case class Var(name: String) extends SimpleValue
// also Bool literals, see below.

sealed trait Keyword extends Token
case object Let extends Keyword

trait BoolLiteral extends Keyword with SimpleValue
case object True extends BoolLiteral
case object False extends BoolLiteral

sealed trait Syntax extends Token
case object OpenParen extends Syntax
case object CloseParen extends Syntax

sealed case class Operator(symbol: String, signature: BinopType) extends Token
object Plus extends Operator("+", BinopType(NumType, NumType, NumType)) {}
object Star extends Operator("*", BinopType(NumType, NumType, NumType)) {}