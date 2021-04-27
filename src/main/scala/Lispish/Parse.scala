import util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Success
import scala.util.Try
object Parse extends PackratParsers {
  type Elem = Token
  def number: Parser[NumLiteral] = {
    accept("number", { case (n: NumLiteral) => n })
  }

  def bool: Parser[BoolLiteral] =
    accept("boolean literal", { case (b: BoolLiteral) => b })

  def variable: Parser[Var] = {
    accept("variable", { case v @ Var(name) => v })
  }

  def simpleValue: Parser[SimpleValue] = {
    bool | number | variable
  }

  def operator: Parser[Operator] =
    accept("operator", { case (o: Operator) => o })

  def mathExpr: Parser[BinopExpr] = {
    (simpleValue | bracketedExpr) ~ operator ~ expression ^^ {
      case expr1 ~ op ~ expr2 =>
        BinopExpr(op, expr1, expr2)
    }
  }

  def letExpr: Parser[LetExpr] = {
    Let ~ bindings ~ expression ^^ { case _let ~ bs ~ targetExpression =>
      LetExpr(bs, targetExpression)
    }
  }

  def bracketedExpr: Parser[Expression] = {
    OpenParen ~ expression ~ CloseParen ^^ { case _ ~ expr ~ _ =>
      expr
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
    mathExpr | letExpr | bracketedExpr | simpleValue
  }

  def program = {
    phrase(expression)
  }

  def apply(tokens: List[Token]): Try[Expression] = {
    val reader = new SeqReader(tokens)
    program(reader) match {
      case Success(result, next) => scala.util.Success(result)
      case NoSuccess(msg, next) =>
        scala.util.Failure(
          ParserException(s"parsing failed on $tokens, \n$msg \n$next")
        )
    }
  }

  def apply(tokens: Try[List[Token]]): Try[Expression] = {
    tokens.flatMap(apply)
  }
}
