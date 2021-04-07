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

  test("Can parse arithmetic expressions") {
    assert(Solution.parse("(+ 1 2)") == Binop(Plus, Num(1), Num(2)))
  }

  test("Can parse single let bindings") {
    assert(
      Solution.parse("(let x 2 x)") == LetExpr(
        List((Var("x"), Num(2))),
        Var("x")
      )
    )
    assert(
      Solution.parse("(let x 2 (let y 12 (+ x y)))") == LetExpr(
        List((Var("x"), Num(2))),
        LetExpr(
          List(
            (Var("y"), Num(12))
          ),
          Binop(Plus, Var("x"), Var("y"))
        )
      )
    )
  }

  test("Can parse multiple let bindings") {
    assert(
      Solution.parse("(let x 2 y 12 x)") == LetExpr(
        List(
          (Var("x"), Num(2)),
          (Var("y"), Num(12))
        ),
        Var("x")
      )
    )
    assert(
      Solution.parse("(let x 2 (let y 12 (+ x y)))") == LetExpr(
        List((Var("x"), Num(2))),
        LetExpr(
          List(
            (Var("y"), Num(12))
          ),
          Binop(Plus, Var("x"), Var("y"))
        )
      )
    )
  }

  test("get addition and multiplication right") {
    assert(Solution.interpret("(+ 7 13)") == 20)
    assert(Solution.interpret("(* 7 13)") == 91)
  }

  test("Evaluates simple expressions correctly") {
    assert(Solution.interpret("(+ 1 2)") == 3)
    println(Solution.interpret("(* (let x 3 x) 12)"))
    assert(Solution.interpret("(* (let x 3 x) 12)") == 36)

  }
  test("handles nested let bindings correctly") {
    assert(
      Solution.interpret(
        "(let x 7 (+ (let x 30 x) 1700))"
      ) == 1730
    )
  }

  test("handles multiple bindings in let") {
    Solution.interpret(
      "(let x 7 y 13 (+ (let y 30 x 1700 (+ x y)) y))"
    ) == 1743
  }

}
