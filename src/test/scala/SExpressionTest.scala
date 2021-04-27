class SExpressionTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Can parse a simple value.") {
    assert(RecordScript.parse("130").get == NumLiteral(130))
    assert(RecordScript.parse("x").get == Var("x"))
  }

  test("Can parse arithmetic expressions") {
    assert(
      RecordScript
        .parse("1 + 2")
        .get == BinopExpr(Plus, NumLiteral(1), NumLiteral(2))
    )
  }

  test("Can parse single let bindings") {
    assert(
      RecordScript.parse("(let x 2 x)").get == LetExpr(
        List((Var("x"), NumLiteral(2))),
        Var("x")
      )
    )
    assert(
      RecordScript.parse("(let x 2 (let y 12 (x + y)))").get == LetExpr(
        List((Var("x"), NumLiteral(2))),
        LetExpr(
          List(
            (Var("y"), NumLiteral(12))
          ),
          BinopExpr(Plus, Var("x"), Var("y"))
        )
      )
    )
  }

  test("Can parse multiple let bindings") {
    assert(
      RecordScript.parse("(let x 2 y 12 x)").get == LetExpr(
        List(
          (Var("x"), NumLiteral(2)),
          (Var("y"), NumLiteral(12))
        ),
        Var("x")
      )
    )
    assert(
      RecordScript.parse("(let x 2 (let y 12 (x + y)))").get == LetExpr(
        List((Var("x"), NumLiteral(2))),
        LetExpr(
          List(
            (Var("y"), NumLiteral(12))
          ),
          BinopExpr(Plus, Var("x"), Var("y"))
        )
      )
    )
  }

  test("get addition and multiplication right") {
    assert(RecordScript.interpret("(7 + 13)").get == 20)
    assert(RecordScript.interpret("(7 * 13)").get == 91)
  }

  test("Evaluates simple expressions correctly") {
    assert(RecordScript.interpret("(1 + 2)").get == 3)
    assert(RecordScript.interpret("((let x 3 x) * 12)").get == 36)

  }
  test("handles nested let bindings correctly") {
    assert(
      RecordScript
        .interpret(
          "(let x 7 ((let x 30 x) + 1700))"
        )
        .get == 1730
    )
  }

  test("handles multiple bindings in let") {
    assert(
      RecordScript
        .interpret(
          "(let x 7 y 13 ((let y 30 x 1700 (x + y)) + y))"
        )
        .get == 1743
    )
  }

  test("can interpret a file") {
    assert(
      RecordScript
        .interpretFile(
          "src/test/scala/examples/simpleProgram.sexp"
        )
        .get == 1743
    )
  }

  test("extra parens are okay") {
    assert(RecordScript.interpret("(((1)))").get == 1)
  }

}
