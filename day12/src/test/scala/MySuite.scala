// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day12 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "7163")
    assertEquals(score2, "17788038834112")

  test("Day12 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "21")
    assertEquals(score2, "525152")
