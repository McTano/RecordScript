class BLTest extends org.scalatest.funsuite.AnyFunSuite {
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
