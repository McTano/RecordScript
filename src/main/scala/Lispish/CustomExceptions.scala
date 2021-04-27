sealed class RecordScriptException(message: String) extends Exception(message)

class SyntaxError(message: String) extends RecordScriptException(message)

case class ParserException(message: String)
    extends RecordScriptException(message)
sealed class RecordScriptTypeError(message: String)
    extends RecordScriptException(message)

class OperatorTypeMismatch(op: Operator, expected: Type, actual: Type)
    extends RecordScriptTypeError(
      s"Operator $op expects $expected on right-hand side. received $actual"
    )
case class UnboundVarLookup(v: Var, context: Context[Type])
    extends RecordScriptTypeError(
      s"We Can't find a type for variable $v in context $context"
    )
