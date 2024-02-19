// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day8 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "22357")
    assertEquals(score2, "10371555451871")

  test("Day8 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "2")
    assertEquals(score2, "2")
