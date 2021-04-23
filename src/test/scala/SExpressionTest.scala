class SExpressionTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Can parse a simple value.") {
    assert(Lispish.parse("130") == Num(130))
    assert(Lispish.parse("x") == Var("x"))
  }

  test("Can parse arithmetic expressions") {
    assert(Lispish.parse("1 + 2") == Binop(Plus, Num(1), Num(2)))
  }

  test("Can parse single let bindings") {
    assert(
      Lispish.parse("(let x 2 x)") == LetExpr(
        List((Var("x"), Num(2))),
        Var("x")
      )
    )
    assert(
      Lispish.parse("(let x 2 (let y 12 (x + y)))") == LetExpr(
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
      Lispish.parse("(let x 2 y 12 x)") == LetExpr(
        List(
          (Var("x"), Num(2)),
          (Var("y"), Num(12))
        ),
        Var("x")
      )
    )
    assert(
      Lispish.parse("(let x 2 (let y 12 (x + y)))") == LetExpr(
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
    assert(Lispish.interpret("(7 + 13)") == 20)
    assert(Lispish.interpret("(7 * 13)") == 91)
  }

  test("Evaluates simple expressions correctly") {
    assert(Lispish.interpret("(1 + 2)") == 3)
    assert(Lispish.interpret("((let x 3 x) * 12)") == 36)

  }
  test("handles nested let bindings correctly") {
    assert(
      Lispish.interpret(
        "(let x 7 ((let x 30 x) + 1700))"
      ) == 1730
    )
  }

  test("handles multiple bindings in let") {
    assert(
      Lispish.interpret(
        "(let x 7 y 13 ((let y 30 x 1700 (x + y)) + y))"
      ) == 1743
    )
  }

  test("can interpret a file") {
    assert(
      Lispish.interpretFile(
        "src/test/scala/examples/simpleProgram.sexp"
      ) == 1743
    )
  }

  test("extra parens are okay") {
    assert(Lispish.interpret("(((1)))") == 1)
  }

  test("BL Parser simple example") {
    assert(BLInterpreter("if true then false else true") == false)
  }

  test("BL Parser") {
    assert(
      BLInterpreter(
        "if if true then false else true then false else true"
      ) == true
    )
  }
}
