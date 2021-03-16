class ExpressionTest extends org.scalatest.funsuite.AnyFunSuite {

  val expr = If(
    True,
    False,
    True
  )

  test("Can instantiate an if expression.") {
    println(expr)
    println(expr.eval())
    assert(true)
  }

  test("evaluating If(True,False,True) returns false") {
    assert(If(True, False, True).eval() === False)
  }
}
