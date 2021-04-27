import scala.util.{Try, Success, Failure}
class TypeCheckerTest extends org.scalatest.funsuite.AnyFunSuite {
  test("can typecheck a number") {
    assert(TypeCheck(RecordScript.parse("12")).get == (NumType, NullContext))
  }

  test("can typecheck a simple let binding") {
    assert(
      TypeCheck(RecordScript.parse("let hello 12 hello")) == Success(
        NumType,
        Binding(NullContext, Var("hello"), NumType)
      )
    )
  }

  test("can typecheck a simple addition expression") {
    assert(TypeCheck(RecordScript.parse("112 + 20")).get._1 == NumType)
    assert(
      TypeCheck(RecordScript.parse("let hello 12 hello + hello")) == Success(
        NumType,
        Binding(NullContext, Var("hello"), NumType)
      )
    )

  }

  test("fails with correct error type when rhs is not a number") {
    // TODO when I have another type
  }

  test("catches unbound variable") {
    Try(TypeCheck(RecordScript.parse("x"))) match {
      case Failure(exception) =>
        assert(exception.isInstanceOf[UnboundVarLookup])
      case Success(value) => fail()
    }
  }
}
