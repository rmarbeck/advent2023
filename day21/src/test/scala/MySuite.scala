// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day21 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "3671")
    assertEquals(score2, "609708004316870")

  test("Day21 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "28")
    assertEquals(score2, "528192461129799")
