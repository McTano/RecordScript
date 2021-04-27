import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Try

object Tokenize extends RegexParsers {
  override def skipWhitespace: Boolean = true
  def number: Parser[NumLiteral] = """^(-?\d+)""".r ^^ { case n =>
    NumLiteral(n.toInt)
  }
  def variable: Parser[Var] = """^([a-z]\w*)""".r ^^ { case v =>
    Var(v)
  }
  def openParen: Parser[Syntax] = "(" ^^ (_ => OpenParen)
  def closeParen: Parser[Syntax] = ")" ^^ (_ => CloseParen)
  def let: Parser[Keyword] = "let" ^^ (_ => Let)
  def plus: Parser[Operator] = "+" ^^ (_ => Plus)
  def times: Parser[Operator] = "*" ^^ (_ => Star)
  def bool: Parser[BoolLiteral] = ("true" | "false") ^^ {
    case "true"  => True
    case "false" => False
  }

  def keyword = (let | plus | times | bool)

  def syntax: Parser[Syntax] = { openParen | closeParen }

  def token: Parser[Token] = { syntax | keyword | number | variable }
  def tokens: Parser[List[Token]] = { phrase(rep1(token)) }

  def apply(program: String): Try[List[Token]] = {
    val reader = new CharSequenceReader(program)
    parse(tokens, reader) match {
      case Success(result, next) => scala.util.Success(result)
      case NoSuccess(msg, _) =>
        scala.util.Failure(new ParserException(msg))
    }
  }
}
