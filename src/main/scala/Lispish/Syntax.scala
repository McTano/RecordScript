sealed trait Expression
case class LetExpr(
    binders: List[(Var, Expression)],
    targetExpression: Expression
) extends Expression
case class BinopExpr(o: Operator, e1: Expression, e2: Expression)
    extends Expression

sealed trait Token

sealed trait SimpleValue extends Token with Expression
case class NumLiteral(n: Double) extends SimpleValue
case class Var(name: String) extends SimpleValue
case class StringLiteral(chars: String) extends SimpleValue
// also Bool literals, see below.

sealed trait Keyword extends Token
case object Let extends Keyword

trait BoolLiteral extends Keyword with SimpleValue
case object True extends BoolLiteral
case object False extends BoolLiteral

sealed trait Syntax extends Token
case object OpenParen extends Syntax
case object CloseParen extends Syntax

case object OpenBrace extends Syntax
case object CloseBrace extends Syntax

case object OpenBracket extends Syntax
case object CloseBracket extends Syntax

case object SemiColon extends Syntax
case object Comma extends Syntax

sealed case class Operator(symbol: String, signature: BinopType) extends Token
object Plus extends Operator("+", BinopType(NumType, NumType, NumType)) {}
object Star extends Operator("*", BinopType(NumType, NumType, NumType)) {}
