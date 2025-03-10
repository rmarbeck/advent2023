// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day23b : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "2186")
    assertEquals(score2, "0")

  test("Day23b : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "94")
    assertEquals(score2, "0")
