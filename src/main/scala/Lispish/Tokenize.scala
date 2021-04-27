import scala.util.parsing.combinator._
import scala.util.parsing.input._

object Tokenize extends RegexParsers {
  override def skipWhitespace: Boolean = true
  def number: Parser[Num] = """^(-?\d+)""".r ^^ { case n => Num(n.toInt) }
  def variable: Parser[Var] = """^([a-z]\w*)""".r ^^ { case v => Var(v) }
  def openParen: Parser[Syntax] = "(" ^^ (_ => OpenParen)
  def closeParen: Parser[Syntax] = ")" ^^ (_ => CloseParen)
  def let: Parser[Keyword] = "let" ^^ (_ => Let)
  def plus: Parser[Operator] = "+" ^^ (_ => Plus)
  def times: Parser[Operator] = "*" ^^ (_ => Star)

  def keyword = (let | plus | times)

  def syntax: Parser[Syntax] = { openParen | closeParen }

  def token: Parser[Token] = { syntax | keyword | number | variable }
  def tokens: Parser[List[Token]] = { phrase(rep1(token)) }

  def apply(program: String): List[Token] = {
    val reader = new CharSequenceReader(program)
    parse(tokens, reader) match {
      case Success(result, next) => result
      case NoSuccess(msg, _) =>
        throw new ParserException(msg)
    }
  }
}