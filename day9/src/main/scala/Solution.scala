import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val listsOfValues = inputLines.map:
      case values => values.split(" ").map(_.toLong).toList

    val List(resultPart1, resultPart2) =
      List(listsOfValues, listsOfValues.map(_.reverse)).map:
        case currentList => currentList.map(solve(_)).sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def solve(values: List[Long], previousSum: Long = 0l) : Long =
  values.forall(_ == 0l) match
    case true => previousSum
    case false =>
      val differences =
        values.sliding(2, 1).map:
          case List(first, second) => second - first
          case value => throw Exception(s"Not managed : $value")

      solve(differences.toList, values.last + previousSum)
