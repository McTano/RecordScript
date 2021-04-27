import scala.util.{Try, Success, Failure}
class TypeCheckerTest extends org.scalatest.funsuite.AnyFunSuite {
  test("can typecheck a number") {
    assert(RecordScript.parseAndCheck("12").get == (NumType, NullContext))
  }

  test("can typecheck a simple let binding") {
    assert(
      RecordScript.parseAndCheck("let hello 12 hello") == Success(
        NumType,
        Binding(NullContext, Var("hello"), NumType)
      )
    )
  }

  test("can typecheck a simple addition expression") {
    assert(RecordScript.parseAndCheck("112 + 20").get._1 == NumType)
    assert(
      RecordScript.parseAndCheck("let hello 12 hello + hello") == Success(
        NumType,
        Binding(NullContext, Var("hello"), NumType)
      )
    )

  }

  test("fails with correct error type when rhs is not a number") {
    assertThrows[OperatorTypeMismatch](
      RecordScript.parseAndCheck("-1030 + false").get
    )
    assertThrows[OperatorTypeMismatch](
      RecordScript.parseAndCheck("1 + true").get
    )
  }

  test("catches unbound variable") {
    assertThrows[UnboundVarLookup](RecordScript.parseAndCheck("x").get)
    assertThrows[UnboundVarLookup](RecordScript.parseAndCheck("x + 1").get)
    assertThrows[UnboundVarLookup](RecordScript.parseAndCheck("12 * x").get)
  }

  test("trivial boolean checks") {
    assert(RecordScript.parseAndCheck("true").get == (BoolType, NullContext))
    assert(
      RecordScript.parseAndCheck("false").get == (BoolType, NullContext)
    )
  }
}
