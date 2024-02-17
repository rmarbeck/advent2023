// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day5 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "462648396")
    assertEquals(score2, "2520479")

  test("Day5 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "35")
    assertEquals(score2, "46")
