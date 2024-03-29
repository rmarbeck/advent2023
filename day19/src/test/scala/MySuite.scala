// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day19 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "386787")
    assertEquals(score2, "131029523269531")

  test("Day19 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "19114")
    assertEquals(score2, "167409079868000")
