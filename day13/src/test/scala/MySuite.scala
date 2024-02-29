// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day13 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "34772")
    assertEquals(score2, "35554")

  test("Day13 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "405")
    assertEquals(score2, "400")
