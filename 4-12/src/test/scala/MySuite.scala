// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("main test"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "26443")
    assertEquals(score2, "6284877")
