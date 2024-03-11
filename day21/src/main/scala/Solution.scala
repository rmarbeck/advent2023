import scala.annotation.tailrec

val cacheActive = false

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    given garden: Garden = Garden.from(inputLines)

    val resultPart1 = count(List(garden.startingPosition), 64)

    given AdvancedIGarden = AdvancedIGarden(garden)
    val steps: Long = 26_501_365
    val resultPart2 = Suite.calc(steps)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

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

case class Suite(u0: Long, u1: Long, factor: Long):
  private def calc(n:Long): Long = n*u1 - (n - 1) *u0 + (n*(n-1)/2)*factor

object Suite:
  def calc(steps: Long)(using AdvancedIGarden): Long =
    val sizeOfInitialGarden = summon[AdvancedIGarden].garden.height
    val halfSize = sizeOfInitialGarden / 2
    val stepsToWalk: Long = (steps - halfSize) / sizeOfInitialGarden
    val firstRound = sizeOfInitialGarden + halfSize
    val secondRound = 2 * sizeOfInitialGarden + halfSize

    guessTerms(List(halfSize, firstRound, secondRound)).calc(stepsToWalk)

  def guessTerms(terms: List[Int])(using AdvancedIGarden): Suite =
    val List(u2, u1, u0) = summon[AdvancedIGarden].guessTerms(terms)
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


case class AdvancedIGarden(garden: Garden):
  private lazy val startingPosition: Position = garden.startingPosition
  private def slide(position: Position): Position =
    def signedModulo(value: Int, modulo: Int) =
      value match
        case current if current % modulo < 0 => (current % modulo + modulo)
        case current => (value % modulo)
    Position(signedModulo(position.row, garden.height), signedModulo(position.col, garden.width))

  def guessTerms(terms: List[Int]): List[Long] =
    import scala.collection.mutable.Map
    val visited: Map[Position, Boolean] = Map(this.startingPosition -> true)
    def alreadyVisited(position: Position): Boolean =
      visited.contains(position) match
        case false =>
          visited.put(position, true)
          false
        case true => true
    def nextFrontierOnly(position: Position): Set[Position] =
      Set(position.north, position.south, position.east, position.west).filterNot(isARock).filterNot(alreadyVisited)

    @tailrec
    def countAdvanced(frontier: Set[Position], previous: Int, beforePrevious: Int, toFind: List[Int], results: List[Long] = Nil, counter: Int = 0): List[Long] =
      toFind match
        case Nil => results
        case head :: tail =>
          val nextFrontier = frontier.flatMap(nextFrontierOnly)
          val nextFrontierSize = nextFrontier.size
          counter match
            case value if value == head => countAdvanced(nextFrontier, beforePrevious + frontier.size, previous, tail, (beforePrevious + frontier.size) +: results, counter + 1)
            case _ => countAdvanced(nextFrontier, beforePrevious + frontier.size, previous, toFind, results, counter + 1)


    countAdvanced(Set(this.startingPosition), 0, 0, terms)

  private def isARock(position: Position): Boolean = garden.isARock(slide(position))
