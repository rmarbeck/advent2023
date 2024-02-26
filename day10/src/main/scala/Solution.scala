import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result = Field(inputLines).loopSize

    val result1 = s"$result"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

type Tile = Special | Connector

enum Special:
  case Start, Ground
export Special.*

enum Connector:
  case NorthSouth, EastWest, NorthEast, NorthWest, SouthWest, SouthEast
export Connector.*

case class Coords(row: Int, col: Int):
  lazy val north = this.copy(row = row - 1)
  lazy val south = this.copy(row = row + 1)
  lazy val east = this.copy(col = col + 1)
  lazy val west = this.copy(col = col - 1)

  def isDefined(using field: Field): Boolean =
    row >= 0 && row <= field.height - 1  && col >= 0  && col <= field.width - 1

case class Field(input: Seq[String]):
  val data: Array[Array[Tile]] =
    input.toArray.map:
      _.toCharArray.map:
        case 'S' => Start
        case '.' => Ground
        case '|' => NorthSouth
        case '-' => EastWest
        case 'L' => NorthEast
        case 'J' => NorthWest
        case '7' => SouthWest
        case 'F' => SouthEast
        case _ => throw Exception("Not managed")

  lazy val height: Int = data.length
  lazy val width: Int = data(0).length

  def valueAt(position: Coords): Tile = data(position.row)(position.col)

  def next(from: Coords): List[Coords] =
    def manageStart: List[Coords] =
      given Field = this

      val toStartAt = List(from.north, from.east, from.south, from.west).zipWithIndex.filter(_._1.isDefined).map:
        (coords, index) => (coords, valueAt(coords), index)
      .find:
          case (_, NorthSouth | SouthWest | SouthEast, 0) => true
          case (_, NorthSouth | NorthEast | NorthWest, 2) => true
          case (_, EastWest | NorthWest | SouthWest, 1) => true
          case (_, EastWest | NorthEast | SouthEast, 3) => true
          case _ => false
      .map(_._1)

      List(toStartAt.get)

    valueAt(from) match
      case Start => manageStart
      case NorthSouth => List(from.north, from.south)
      case EastWest => List(from.east, from.west)
      case NorthEast => List(from.north, from.east)
      case NorthWest => List(from.north, from.west)
      case SouthWest => List(from.south, from.west)
      case SouthEast => List(from.south, from.east)
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

