import scala.annotation.tailrec
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val tiles = inputLines.map(_.toCharArray.map(Tile.from)).toArray

    val (height, width) = (tiles.length, tiles(0).length)
    val rowBeams = (0 until height).flatMap:
      currentRow => List(Beam(currentRow, -1, WestToEast), Beam(currentRow, width, EastToWest))
    val colBeams = (0 until width).flatMap:
      currentCol => List(Beam(-1, currentCol, NorthToSouth), Beam(height, currentCol, SouthToNorth))

    val beams = rowBeams ++: colBeams

    val results = beams.par.map(count(_)(using PlayField(tiles)))

    val result1 = s"${results.head}"
    val result2 = s"${results.max}"

    (s"${result1}", s"${result2}")

end Solution

def count(beam: Beam)(using playField: PlayField): Int = count(List(beam))

@tailrec
def count(beams: List[Beam])(using playField: PlayField): Int =
  beams match
    case Nil => playField.energy
    case head :: tail => count(playField.energize(head) ::: tail)

enum Tile:
  case Empty, Horizontal, Vertical, Slash, Antislash

object Tile:
  def nextDir(tile: Tile, direction: Direction): Direction =
    (tile, direction) match
      case (Slash, SouthToNorth) | (Antislash, NorthToSouth) => WestToEast
      case (Slash, NorthToSouth) | (Antislash, SouthToNorth) => EastToWest
      case (Slash, EastToWest) | (Antislash, WestToEast) => NorthToSouth
      case (Slash, WestToEast) | (Antislash, EastToWest) => SouthToNorth
      case _ => direction

  def from(char: Char): Tile =
    char match
      case '.' => Empty
      case '-' => Horizontal
      case '|' => Vertical
      case '/' => Slash
      case '\\' => Antislash
      case _ => throw Exception("Unrecognized Tile")

export Tile.*

enum Direction:
  case SouthToNorth, NorthToSouth, EastToWest, WestToEast

export Direction.*

case class EnergetisationStatus(fromNorth: Boolean = false, fromEast: Boolean = false, fromSouth: Boolean = false, fromWest: Boolean = false):
  lazy val isTrue = fromNorth || fromEast || fromSouth || fromWest
  def energetizeFrom(direction: Direction): EnergetisationStatus =
    direction match
      case SouthToNorth => this.copy(fromSouth = true)
      case NorthToSouth => this.copy(fromNorth = true)
      case EastToWest => this.copy(fromEast = true)
      case WestToEast => this.copy(fromWest = true)

case class Place(tile: Tile, energetisationStatus: EnergetisationStatus = EnergetisationStatus()):
  def isEnergizedFrom(direction: Direction): Boolean =
    direction match
      case SouthToNorth => energetisationStatus.fromSouth
      case NorthToSouth => energetisationStatus.fromNorth
      case EastToWest => energetisationStatus.fromEast
      case WestToEast => energetisationStatus.fromWest

  override def toString: String =
    energetisationStatus.isTrue match
      case true => "#"
      case false => "."

case class Position(row: Int, col: Int)

case class Beam(position: Position, direction: Direction):
  def next: Position =
    direction match
      case SouthToNorth => position.copy(row = position.row - 1)
      case NorthToSouth => position.copy(row = position.row + 1)
      case EastToWest => position.copy(col = position.col - 1)
      case WestToEast => position.copy(col = position.col + 1)

object Beam:
  def apply(row: Int, col: Int, direction: Direction) = new Beam(Position(row, col), direction)

class PlayField(tiles: Array[Array[Tile]]):
  val (height, width) = (tiles.length, tiles(0).length)
  val statuses: Array[Array[EnergetisationStatus]] = Array.fill(height, width)(EnergetisationStatus())

  def isDefinedAt(position: Position): Boolean = tiles.isDefinedAt(position.row) && tiles(position.row).isDefinedAt(position.col)
  def getPlace(position: Position): Place = Place(tiles(position.row)(position.col), statuses(position.row)(position.col))
  def updatePlace(position: Position, newStatus: EnergetisationStatus): Unit = statuses(position.row)(position.col) = newStatus
  def energize(beam: Beam): List[Beam] =
    beam.next match
      case next if ! isDefinedAt(next) => Nil
      case next =>
        (getPlace(next), beam.direction) match
          case (place, direction) if place.isEnergizedFrom(direction) => Nil
          case (place, direction) =>
            updatePlace(next, place.energetisationStatus.energetizeFrom(direction))
            (place.tile, direction) match
              case (Empty, _) | (Horizontal, EastToWest | WestToEast) | (Vertical, SouthToNorth | NorthToSouth) => List(beam.copy(position = next))
              case (Horizontal, _)  => List(beam.copy(position = next, direction = EastToWest), beam.copy(position = next, direction = WestToEast))
              case (Vertical, _)  => List(beam.copy(position = next, direction = SouthToNorth), beam.copy(position = next, direction = NorthToSouth))
              case (tile, dir) => List(beam.copy(position = next, direction = Tile.nextDir(tile, dir)))

  def energy: Int = statuses.flatten.count(_.isTrue)

  override def toString: String = statuses.map(_.mkString).mkString("\n")