// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "454")
    assertEquals(score2, "74287")

  test("main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "5")
    assertEquals(score2, "7")