import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Try
object Tokenize extends RegexParsers {

  def EofCh = CharArrayReader.EofCh

  override def skipWhitespace: Boolean = true
  def number: Parser[NumLiteral] = """-?\d+(\.\d*)?""".r ^^ { case nstr =>
    NumLiteral(nstr.toDouble)
  }

  def variable: Parser[Var] = """^([a-z]\w*)""".r ^^ {
    Var(_)
  }

  def string: Parser[StringLiteral] = (""""(.*?)(?<!\\)"""".r ^^ {
    case s""""${contents}"""" => StringLiteral(contents)
  }) | ("""'(.*?)(?<!\\)'""".r ^^ { case s"""'${contents}'""" =>
    StringLiteral(contents)
  })

  def openParen: Parser[Syntax] = "(" ^^ { _ => OpenParen }
  def closeParen: Parser[Syntax] = ")" ^^ { _ => CloseParen }

  def openBrace: Parser[Syntax] = "{" ^^ { _ => OpenBrace }
  def closeBrace: Parser[Syntax] = "}" ^^ { _ => CloseBrace }

  def openBracket: Parser[Syntax] = "[" ^^ { _ => OpenBracket }
  def closeBracket: Parser[Syntax] = "]" ^^ { _ => CloseBracket }

  def semiColon: Parser[Syntax] = ";" ^^ { _ => SemiColon }

  def comma: Parser[Syntax] = "," ^^ { _ => Comma }

  def let: Parser[Keyword] = "let" ^^ (_ => Let)
  def plus: Parser[Operator] = "+" ^^ (_ => Plus)
  def times: Parser[Operator] = "*" ^^ (_ => Star)
  def bool: Parser[BoolLiteral] = ("true" | "false") ^^ {
    case "true"  => True
    case "false" => False
  }

  def operator: Parser[Operator] = plus | times

  def keyword: Parser[Keyword] = (let | bool)

  def syntax: Parser[Syntax] = {
    openParen | closeParen | openBrace | closeBrace | openBracket | closeBracket | semiColon | comma
  }

  def token: Parser[Token] = {
    syntax | keyword | operator | string | number | variable
  }
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
