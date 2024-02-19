import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val moves = inputLines.head
    val network = HashMap[String, LeftOrRight](inputLines.tail.filterNot(_.isEmpty).map:
      case s"$key = ($left, $right)" => key -> LeftOrRight(left, right)
    : _*)

    val resultPart1 = search(network, "AAA", moves)

    val part2Start = network.keys.filter(_.last == 'A').toList
    val resultPart2 = lowestProduct {
      part2Start.map(search(network, _, moves))
    }

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def search(in: Map[String, LeftOrRight], from: String, via: String, inter: Long = 0) : Long =
  from.last == 'Z' match
    case true => inter
    case false =>
      val currentMove = via.charAt((inter%via.size).toInt)
      val nextFrom = in.get(from) match
          case Some(value: LeftOrRight) => value.extract(currentMove)
          case None => throw Exception(s"Key not found $from")

      search(in, nextFrom, via, inter + 1)

enum LorR:
  case Left, Right

object LorR:
  def from(char: Char) =
    char match
      case 'L' => Left
      case 'R' => Right
      case _ => throw Exception("Unsupported move")

export LorR.*

case class LeftOrRight(left: String, right: String):
  private def extract(move: LorR): String =
    move match
      case Left => left
      case Right => right
  def extract(move: Char): String = extract(LorR.from(move))


def gcd(first: Long, second: Long): Long =
  (first, second) match
    case (0, value) => value
    case (xValue, yValue) if xValue < 0 => gcd(-xValue, yValue)
    case (xValue, yValue) if yValue < 0 => -gcd(xValue, -yValue)
    case (xValue, yValue) => gcd(yValue % xValue, xValue)

def lowestProduct(values: List[Long]): Long =
  values match
    case head :: Nil => head
    case head :: tail =>
      val gcds =
        tail.map:
          case currentFromTail => gcd(head, currentFromTail)

      val commonGcd = gcds.distinct.length match
        case 1 => gcds(0)
        case _ => throw Exception(s"No common GCD found")

      values.map(_ / commonGcd).product * commonGcd
    case _ => throw Exception(s"Not managed")