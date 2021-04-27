class RecordScriptException(message: String) extends Exception(message)

class ParserException(message: String) extends RecordScriptException(message)

sealed class TypeException(msg: String) extends RecordScriptException(msg)
class OperatorTypeMismatch(op: Operator, expected: Type, actual: Type)
    extends TypeException(
      s"Operator $op expects $expected on right-hand side. received $actual"
    )
case class UnboundVarLookup(v: Var, context: Context[Type])
    extends TypeException(
      s"We Can't find a type for variable $v in context $context"
    )
