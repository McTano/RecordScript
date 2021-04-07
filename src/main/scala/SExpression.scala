import util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Success

object Solution {
  val binops: Map[Operator, ((Int, Int) => Int)] =
    Map(Plus -> (_ + _), Star -> (_ * _))

  def interpret(expression: String): Int = {
    evaluate(parse(expression), NullContext)
  }

  def evaluate(expression: Expression, context: Context): Int = {
    expression match {
      case Num(n) => n
      case Var(v) =>
        context.lookup(v) match {
          case None =>
            throw new RuntimeException(
              s"variable $v not found in context: $context"
            )
          case Some(value) => value
        }
      case LetExpr(binders, targetExpression) =>
        evaluate(
          targetExpression,
          binders.foldLeft(context)((context, binder) =>
            context.extend(binder._1.name, evaluate(binder._2, context))
          )
        )
      case Binop(op, lhs, rhs) =>
        binops(op)(evaluate(lhs, context), evaluate(rhs, context))
      case _ =>
        throw new RuntimeException(s"expression `$expression not recognized`")
    }
  }

  def parse(str: String): Expression = {
    SParsers(SLexer(str))
  }
}

class SeqReader[T](seq: Seq[T]) extends Reader[T] {
  def first = seq.head
  def atEnd: Boolean = seq.isEmpty
  def pos: Position = NoPosition
  def rest: SeqReader[T] = new SeqReader(seq.tail)
}

sealed trait Expression
case class LetExpr(
    binders: List[(Var, Expression)],
    targetExpression: Expression
) extends Expression
case class Binop(o: Operator, e1: Expression, e2: Expression) extends Expression
case class MultExpr(e1: Expression, e2: Expression) extends Expression

sealed trait Token

sealed trait SimpleValue extends Token with Expression
case class Num(n: Int) extends SimpleValue
case class Var(name: String) extends SimpleValue

sealed trait Syntax extends Token
case object OpenParen extends Syntax
case object CloseParen extends Syntax

sealed trait Keyword extends Token
case object Let extends Keyword

sealed trait Operator extends Token
case object Plus extends Operator
case object Star extends Operator

sealed trait Context {
  def extend(b: String, v: Int): Context = {
    new Binding(this, b, v)
  }
  def lookup(v: String): Option[Int]
}
case object NullContext extends Context {
  def lookup(v: String): Option[Int] = {
    None
  }
}
case class Binding(outerContext: Context, variable: String, value: Int)
    extends Context {
  def lookup(v: String): Option[Int] = {
    // println(s"looking up ${v} in context ${this}")
    if (v == variable) {
      Some(value)
    } else {
      outerContext.lookup(v)
    }
  }

  override def toString() = {
    s"""|{
            |  $variable = $value,
            |  outerContext: {
            |    $outerContext
            |  }
            |}""".stripMargin
  }
}
