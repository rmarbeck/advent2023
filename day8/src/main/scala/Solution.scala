import scala.annotation.tailrec
import scala.collection.immutable.HashMap

type Moves = Vector[LorR]
type Key = String
type Network = Map[Key, Alternatives]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    given Moves = inputLines.head.map(LorR.from).toVector
    given network: Network = HashMap.fromSpecific:
      inputLines.tail.collect:
        case s"$key = ($left, $right)" => key -> Alternatives(left, right)

    val result1 = search("AAA")

    val part2Start = network.keys.withFilter(_.last == 'A')
    val result2 = lowestProduct:
      part2Start.map(search(_)).toList

    (s"$result1", s"$result2")

@tailrec
def search(from: Key, steps: Long = 0)(using network: Network, moves: Moves) : Long =
  if from.last == 'Z' then
    steps
  else
    val currentMove: LorR = moves((steps % moves.size).toInt)
    val nextFrom: Key = network(from).extract(currentMove)

    search(nextFrom, steps + 1)

enum LorR:
  case Left, Right

object LorR:
  def from(char: Char): LorR =
    char match
      case 'L' => Left
      case 'R' => Right
      case _ => throw Exception("Unsupported move")

import LorR.*

case class Alternatives(left: String, right: String):
  def extract(move: LorR): Key =
    move match
      case Left => left
      case Right => right

@tailrec
def gcd(first: Long, second: Long): Long =
  (first, second) match
    case (0, value) => value.abs
    case (xValue, yValue) if xValue < 0 => gcd(-xValue, yValue)
    case (xValue, yValue) => gcd(yValue % xValue, xValue)

def lowestProduct(values: List[Long]): Long =
  values match
    case head :: Nil => head
    case head :: tail =>
      val gcds = tail.toSet.map(gcd(head, _))

      val commonGcd = gcds.size match
        case 1 => gcds.head
        case _ => throw Exception(s"No common GCD found")

      values.map(_ / commonGcd).product * commonGcd
    case _ => throw Exception(s"Not supposed to happen")