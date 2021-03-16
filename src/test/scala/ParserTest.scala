import scala.util.{Try, Success, Failure}

class ParserTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Can parse true") {
    assert(Parser.parse("true") == Success(True))
  }

  test("Can parse false") {
    assert(Parser.parse("false") == Success(False))
  }

  test("Can parse simple conditional") {
    assert(
      Parser.parse("if true then false else true") == Success(
        If(True, False, True)
      )
    )
  }

  test("Fails with correct exception type") {
    Parser.parse("if true then FILSH else true") match {
      case Success(v)                  => assert(false, "should have failed")
      case Failure(e: ParserException) => assert(true)
      case Failure(_: Throwable)       => assert(false, "Wrong kind of error")
    }
  }
}
