// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day16 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "6906")
    assertEquals(score2, "7330")

  test("Day16 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "46")
    assertEquals(score2, "51")
