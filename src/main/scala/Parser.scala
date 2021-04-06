import scala.util.{Try, Success, Failure}
import scala.util.parsing.combinator.Parsers

object Parser {
  def parse(str: String): Try[RSExpression] = {
    Try(parseExpression(str))
  }

  def parseExpression(str: String): RSExpression = {
    str match {
      case "true"  => True
      case "false" => False
      case s"if $condExprStr then $thenExprStr else $elseExprStr" =>
        If(
          parseExpression(condExprStr),
          parseExpression(thenExprStr),
          parseExpression(elseExprStr)
        )
      case _ =>
        throw new ParserException(s"expected expression, found: $str")
    }
  }

}
