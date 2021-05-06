class ParseTest extends org.scalatest.funsuite.AnyFunSuite {
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

  test("Can parse single-quoted string") {
    assert(RecordScript.parse(""""hello"""").get == StringLiteral("hello"))
    assert(RecordScript.parse("""("hello")""").get == StringLiteral("hello"))
    assert(
      RecordScript.parse("""("(goodbye)")""").get == StringLiteral("(goodbye)")
    )
  }

  test("Can parse double quoted string") {
    assert(
      RecordScript
        .parse("'a string can use single quotes'")
        .get == StringLiteral(
        "a string can use single quotes"
      )
    )
  }

  test("single quotes inside double quotes") {
    assert(
      RecordScript.parse("""("it's okay")""").get == StringLiteral("it's okay")
    )
    assert(
      RecordScript
        .parse("""("works when 'single quotes' are paired")""")
        .get == StringLiteral("works when 'single quotes' are paired")
    )
  }

  // test("escaped quotes inside quotes") {
  //   // this test passes, but the escaping isn't working quite right. Looks like the backslashes are getting preserved.
  //   assert(
  //     RecordScript
  //       .parse("""
  //   "don't fail on a \"quote unquote \" \"nested quote\""
  //   """).get
  //       == StringLiteral("don't fail on a \"quote unquote\" \"nested \"quote")
  //   )
  // }

}
