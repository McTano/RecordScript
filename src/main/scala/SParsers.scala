import util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Success
object SParsers extends Parsers {
  type Elem = Token
  def number: Parser[Num] = {
    accept("number", { case n @ Num(_) => n })
  }

  def variable: Parser[Var] = {
    accept("variable", { case v @ Var(_) => v })
  }

  def simpleValue: Parser[SimpleValue] = {
    number | variable
  }

  def operator: Parser[Operator] =
    accept("operator", { case (o: Operator) => o })

  def mathExpr: Parser[Binop] = {
    OpenParen ~ operator ~ expression ~ expression ~ CloseParen ^^ {
      case _ ~ op ~ expr1 ~ expr2 ~ _ => Binop(op, expr1, expr2)
    }
  }

  def letExpr: Parser[LetExpr] = {
    OpenParen ~ Let ~ bindings ~ expression ~ CloseParen ^^ {
      case _ ~ _ ~ bs ~ targetExpression ~ _ => LetExpr(bs, targetExpression)
    }
  }

  def bindings = {
    rep1(variable ~ expression) ^^ {
      _.map(_ match {
        case v ~ e =>
          (v, e)
      })
    }
  }

  def expression = {
    simpleValue | mathExpr | letExpr
  }

  def program = {
    phrase(expression)
  }

  def apply(tokens: List[Token]): Expression = {
    val reader = new SeqReader(tokens)
    program(reader) match {
      case Success(result, next) => result
      case _: NoSuccess =>
        throw new ParserException(s"parsing failed on $tokens")
    }
  }
}
