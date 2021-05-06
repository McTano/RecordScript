import scala.util.parsing.combinator.Parsers

object Unwrap extends Parsers {
  def unwrap[T, ErrorType <: RecordScriptException](
      pResult: ParseResult[T],
      errorConstructor: String => ErrorType
  ): scala.util.Try[T] = {
    pResult match {
      case Success(res, next) => scala.util.Success(res)
      case Failure(msg, next) =>
        scala.util.Failure(errorConstructor(msg))
      case Error(msg, next) => scala.util.Failure(errorConstructor(msg))
    }
  }
}
