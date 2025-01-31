import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val NumbersExt = """(-?\d+)""".r
    val listsOfValues = inputLines.map:
      NumbersExt.findAllIn(_).toVector.map(_.toLong)

    val List(result1, result2) =
      List(listsOfValues, listsOfValues.map(_.reverse)).map:
        currentList => currentList.map(solve(_)).sum

    (s"$result1", s"$result2")

@tailrec
def solve(values: Vector[Long], previousSum: Long = 0L) : Long =
  if values.forall(_ == 0L) then
    previousSum
  else
    val differences =
      values.sliding(2, 1).collect:
        case Vector(first, second) => second - first
      .toVector

    solve(differences, values.last + previousSum)
