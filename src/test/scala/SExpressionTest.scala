class SExpressionTest extends org.scalatest.funsuite.AnyFunSuite {

  val expr = If(
    True,
    False,
    True
  )

  test("Can parse a simple value.") {
    assert(Solution.parse("130") == Num(130))
    assert(Solution.parse("x") == Var("x"))
  }

}
