import util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Success
import scala.io.Source
import java.io.File

object RecordScript {
  val binops: Map[Operator, ((Int, Int) => Int)] =
    Map(Plus -> (_ + _), Star -> (_ * _))

  def interpret(program: String): Int = {
    evaluate(parse(program), NullContext)
  }

  def interpretFile(path: String): Int = {
    val fBuffer = Source.fromFile(new File(path))
    val program: String = fBuffer.getLines.mkString
    fBuffer.close
    interpret(program)
  }

  def evaluate(expression: Expression, context: Context[Int]): Int = {
    expression match {
      case Num(n) => n
      case (v: Var) =>
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
            context.extend(binder._1, evaluate(binder._2, context))
          )
        )
      case Binop(op, lhs, rhs) =>
        binops(op)(evaluate(lhs, context), evaluate(rhs, context))
      case _ =>
        throw new RuntimeException(s"expression `$expression not recognized`")
    }
  }

  def parse(str: String): Expression = {
    Parse(Tokenize(str))
  }
}

class SeqReader[T](seq: Seq[T]) extends Reader[T] {
  def first = seq.head
  def atEnd: Boolean = seq.isEmpty
  def pos: Position = NoPosition
  def rest: SeqReader[T] = new SeqReader(seq.tail)
  override def toString = seq.toString
}

sealed trait Expression
case class LetExpr(
    binders: List[(Var, Expression)],
    targetExpression: Expression
) extends Expression
case class Binop(o: Operator, e1: Expression, e2: Expression) extends Expression

sealed trait Token

sealed trait SimpleValue extends Token with Expression
case class Num(n: Int) extends SimpleValue
case class Var(name: String) extends SimpleValue

sealed trait Syntax extends Token
case object OpenParen extends Syntax
case object CloseParen extends Syntax

sealed trait Keyword extends Token
case object Let extends Keyword

sealed case class Operator(symbol: String, signature: Fun) extends Token
object Plus extends Operator("+", Fun(NumType, NumType, NumType)) {}
object Star extends Operator("*", Fun(NumType, NumType, NumType)) {}
