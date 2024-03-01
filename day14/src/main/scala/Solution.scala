import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val values = inputLines.map(_.toCharArray).toArray
    val resultPart1 = values.transpose.map(count).sum


    val result1 = s"$resultPart1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

def count(rocks: Array[Char]): Int =
  val size = rocks.size
  count(rocks.toList, size, size, 0)

@tailrec
def count(rocks: List[Char], counter: Int, index: Int, acc: Int): Int =
  rocks match
    case Nil => acc
    case head :: tail =>
      head match
        case 'O' => count(tail, counter - 1, index - 1, acc + counter)
        case '#' => count(tail, index -1, index - 1, acc)
        case '.' => count(tail, counter, index - 1, acc)