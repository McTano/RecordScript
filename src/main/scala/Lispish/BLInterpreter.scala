import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object BLInterpreter extends RegexParsers {

  def TRUE: Parser[Boolean] = "true" ^^ { case _ => true }
  def FALSE: Parser[Boolean] = "false" ^^ { case _ => false }
  def IF: Parser[Boolean] = "if" ^^ { case _ => false }
  def IfThenExpr: Parser[Boolean] = {
    "if" ~ expression ~ "then" ~ expression ~ "else" ~ expression ^^ {
      case _ ~ true ~ _ ~ expr1 ~ _ ~ expr2  => expr1
      case _ ~ false ~ _ ~ expr1 ~ _ ~ expr2 => expr2
    }
  }

  def expression = {
    TRUE | FALSE | IfThenExpr
  }

  def program = {
    phrase(expression)
  }

  def apply(str: String): Boolean = {
    program(new CharSequenceReader(str)) match {
      case Success(result, next) => result
      case Failure(msg, next) =>
        throw new ParserException(s"parsing failed on $str, \n$msg \n$next")
      case Error(msg, next) =>
        throw new ParserException(s"parsing failed on $str, \n$msg \n$next")
    }
  }
}
