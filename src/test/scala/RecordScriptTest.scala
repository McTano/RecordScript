class RecordScriptTest extends org.scalatest.funsuite.AnyFunSuite {

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
