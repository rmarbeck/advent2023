import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result = Field(inputLines).loopSize

    val result1 = s"$result"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

type Tile = Special | Connector

enum Direction:
  case North, East, South, West
export Direction.*

enum Special:
  case Start, Ground
export Special.*

enum Connector(val directions: (Direction, Direction)):
  private case NorthSouth extends Connector((North , South))
  private case EastWest extends Connector((East , West))
  private case NorthEast extends Connector((North , East))
  private case NorthWest extends Connector((North , West))
  private case SouthWest extends Connector((South , West))
  private case SouthEast extends Connector((South , East))

  def connects(direction: Direction): Boolean = directions.toList.exists(_ == direction)

object Connector:
  def unapply(connector: Connector): (Direction, Direction) = connector.directions
  def from(char: Char) =
    char match
      case '|' => NorthSouth
      case '-' => EastWest
      case 'L' => NorthEast
      case 'J' => NorthWest
      case '7' => SouthWest
      case 'F' => SouthEast
      case _ => throw Exception("Not managed")

export Connector.*

case class Coords(row: Int, col: Int):
  lazy val north = this.copy(row = row - 1)
  lazy val south = this.copy(row = row + 1)
  lazy val east = this.copy(col = col + 1)
  lazy val west = this.copy(col = col - 1)

  def move(direction: Direction) =
    direction match
      case North => north
      case East => east
      case South => south
      case West => west

  def isDefined(using field: Field): Boolean =
    row >= 0 && row <= field.height - 1  && col >= 0  && col <= field.width - 1

case class Field(input: Seq[String]):
  private val data: Array[Array[Tile]] =
    input.toArray.map:
      _.toCharArray.map:
        case 'S' => Start
        case '.' => Ground
        case other => Connector.from(other)

  lazy val height: Int = data.length
  lazy val width: Int = data(0).length

  def valueAt(position: Coords): Tile = data(position.row)(position.col)

  def next(from: Coords): List[Coords] =
    def manageStart: List[Coords] =
      given Field = this
      val toStartAt = Direction.values.map(currentDir => (from.move(currentDir), currentDir)).filter(_._1.isDefined).map:
        (coords, direction) => (coords, valueAt(coords), direction)
      .find:
          case (_, connector: Connector, North) if connector.connects(South) => true
          case (_, connector: Connector, East) if connector.connects(West) => true
          case (_, connector: Connector, South) if connector.connects(North) => true
          case (_, connector: Connector, West) if connector.connects(East) => true
          case _ => false
      .map(_._1)

      List(toStartAt.get)

    valueAt(from) match
      case Start => manageStart
      case Connector(firstDir, secondDir) => List(from.move(firstDir), from.move(secondDir))
      case _ => throw Exception("Not managed")

  lazy val loopSize: Int =
    def findStart: Coords =
      val row = data.indexWhere(_.contains(Start))
      val col = data(row).indexWhere(_ == Start)
      Coords(row, col)
    @tailrec
    def populateLoop(start: Coords, inLoop: List[Coords] = List()): List[Coords] =
      inLoop match
        case Nil => populateLoop(start, List(start))
        case head :: tail =>
          val nextCoords: List[Coords] = next(head)
          tail.headOption.map(previous => nextCoords.filterNot(_ == previous)).getOrElse(nextCoords) match
            case Nil => inLoop
            case onlyOne :: Nil =>
              onlyOne == start match
                case true => inLoop
                case false => populateLoop(start, onlyOne +: inLoop)
            case _ => throw Exception(s"Not managed")
    populateLoop(findStart).length / 2

