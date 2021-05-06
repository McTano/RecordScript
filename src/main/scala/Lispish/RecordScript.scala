import util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Success
import scala.io.Source
import java.io.File
import scala.util.Try

object RecordScript {
  val binops: Map[Operator, ((Double, Double) => Double)] =
    Map(Plus -> (_ + _), Star -> (_ * _))

  def interpret(program: String): Try[Double] = {
    parse(program).map(p => evaluate(p, NullContext))
  }

  def readProgram(path: String): Try[String] = {
    Try {
      val fBuffer = Source.fromFile(new File(path))
      val program: String = fBuffer.getLines.mkString
      fBuffer.close
      program
    }
  }

  def interpretFile(path: String): Try[Double] = {
    readProgram(path).flatMap(interpret)
  }

  def evaluate(expression: Expression, context: Context[Double]): Double = {
    expression match {
      case NumLiteral(n) => n
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
      case BinopExpr(op, lhs, rhs) =>
        binops(op)(evaluate(lhs, context), evaluate(rhs, context))
      case _ =>
        throw new RuntimeException(s"expression `$expression not recognized`")
    }
  }

  // Combines the Tokenize and Parse steps
  def parse(str: String): Try[Expression] = {
    Parse(Tokenize(str))
  }

  def parse(tokens: List[Token]): Try[Expression] = {
    Parse(tokens)
  }

  def parseAndCheck(str: String): Try[(Type, Context[Type])] = {
    Tokenize(str).flatMap(Parse.apply).flatMap(TypeCheck.apply)
  }

}

class SeqReader[T](seq: Seq[T]) extends Reader[T] {
  def first = seq.head
  def atEnd: Boolean = seq.isEmpty
  def pos: Position = NoPosition
  def rest: SeqReader[T] = new SeqReader(seq.tail)
  override def toString = seq.toString
}
