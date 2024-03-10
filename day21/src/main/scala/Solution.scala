import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    given garden: Garden = Garden.from(inputLines)

    given igarden: IGarden = IGarden(garden)

    val resultPart1 = count(List(garden.startingPosition), 64)

    val size = igarden.garden.height
    val halfSize = size / 2
    val steps = 26501365

    val stepsToWalk: Long = ((steps - halfSize) / size) - 1

    val resultP2 = Suite.guessTerms(List(halfSize - 1, size + halfSize - 1, 2*size + halfSize - 1)).calc(stepsToWalk)

    val result1 = s"$resultPart1"
    val result2 = s"$resultP2"

    (s"${result1}", s"${result2}")

end Solution

enum Place:
  case Plot, Rock, Starting

object Place:
  def from(char: Char): Place =
    char match
      case  '.' => Plot
      case  '#' => Rock
      case  'S' => Starting

export Place.*

case class Position(row: Int, col: Int):
  def north = this.copy(row = row - 1)
  def south = this.copy(row = row + 1)
  def east = this.copy(col = col - 1)
  def west = this.copy(col = col + 1)

  def next(using garden: Garden): List[Position] =
    List(north, south, east, west).filter(garden.isDefinedAt).filterNot(garden.isARock)

@tailrec
def count(positions: List[Position], remainingSteps: Int)(using Garden): Int =
  remainingSteps match
    case 0 => positions.length
    case steps => count(positions.flatMap(_.next).distinct, remainingSteps - 1)


/*@tailrec
def icount2(positions: List[Position], remainingSteps: Int)(using IGarden): Int =
  remainingSteps match
    case 0 => positions.length
    case steps => icount2(positions.flatMap(_.nextInfinite).distinct, remainingSteps - 1)

def icount(positions: List[Position])(using IGarden): LazyList[Int] =
  val current = positions.length
  current #:: icount(positions.flatMap(_.nextInfinite).distinct)*/

case class Suite(u0: Long, u1: Long, factor: Long):
  def calc(n:Long) = (n+1)*u1 - n*u0 + (n*(n+1)/2)*factor


object Suite:
  def guessTerms(terms: List[Int])(using IGarden): Suite =
    val List(u2, u1, u0) = summon[IGarden].guessTerms(terms)
    val factor = u2 - 2*u1 + u0
    Suite(u0, u1, factor)


case class Garden(places: Array[Array[Place]]):
  lazy val height = places.length
  lazy val width = places(0).length
  lazy val startingPosition: Position =
    val startingRow = places.indexWhere(_.contains(Starting))
    val startingCol = places(startingRow).indexOf(Starting)
    Position(startingRow, startingCol)
  def isDefinedAt(position: Position): Boolean =
    position.row > 0 && position.row < height && position.col > 0 && position.col < width
  def isARock(position: Position): Boolean = places(position.row)(position.col) == Rock

object Garden:
  def from(rawPlaces: Seq[String]): Garden =
    val places =
      rawPlaces.toArray.map:
        line => line.toCharArray.map(Place.from)
    Garden(places)

case class IGarden(garden: Garden):
  import scala.collection.mutable.Map
  val cache: Map[(Int, Int), List[Position]] = Map()
  lazy val startingPosition: Position = garden.startingPosition
  private def slide(position: Position): Position =
    def signedModulo(value: Int, modulo: Int) =
      value match
        case current if current % modulo < 0 => (current % modulo + modulo)
        case current => (value % modulo)
    Position(signedModulo(position.row, garden.height), signedModulo(position.col, garden.width))

  def nextInfinite(position: Position): List[Position] =
    cache.getOrElseUpdate((position.row, position.col), List(position.north, position.south, position.east, position.west).filterNot(isARock))

  def guessTerms(terms: List[Int]): List[Long] =
    @tailrec
    def count(positions: Set[Position], toFind: List[Int], results: List[Long] = Nil, counter: Int = 0): List[Long] =
      toFind match
        case Nil => results
        case head :: tail =>
          val newPositions = positions.flatMap(nextInfinite)
          counter match
            case value if value == head => count(newPositions, tail, newPositions.size +: results, counter + 1)
            case _ => count(newPositions, toFind, results, counter + 1)

    count(Set(this.startingPosition), terms)

  def isARock(position: Position): Boolean = garden.isARock(slide(position))
