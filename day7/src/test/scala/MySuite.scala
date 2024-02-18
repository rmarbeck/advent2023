// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day7 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "248559379")
    assertEquals(score2, "249631254")

  test("Day7 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "6440")
    assertEquals(score2, "5905")
