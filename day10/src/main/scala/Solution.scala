import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val loop = Field(inputLines).loop

    val result1 = s"${loop.size}"
    val result2 = s"${loop.internalArea}"

    (s"$result1", s"$result2")

type Tile = Special | Connector

enum Direction:
  case North, East, South, West
  def opposite: Direction = Direction.fromOrdinal(this.ordinal + 2 % 4)
import Direction.*

enum Special:
  case Start, Ground
import Special.*

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
  def from(char: Char): Connector =
    char match
      case '|' => NorthSouth
      case '-' => EastWest
      case 'L' => NorthEast
      case 'J' => NorthWest
      case '7' => SouthWest
      case 'F' => SouthEast
      case _ => throw Exception("Not managed")

import Connector.*

enum RelativePosition:
  case Above, Below, AtRight, AtLeft
import RelativePosition.*

case class Coordinates(row: Int, col: Int):
  private lazy val north = this.copy(row = row - 1)
  lazy val south: Coordinates = this.copy(row = row + 1)
  lazy val east: Coordinates = this.copy(col = col + 1)
  private lazy val west = this.copy(col = col - 1)

  def move(direction: Direction): Coordinates = direction match
      case North => north
      case East => east
      case South => south
      case West => west

  def isDefined(using field: Field): Boolean =
    row >= 0 && row <= field.height - 1  && col >= 0  && col <= field.width - 1

  def comparedTo(other: Coordinates): RelativePosition = other match
      case `north` => Below
      case `south` => Above
      case `east` => AtLeft
      case `west` => AtRight
      case _ => throw Exception("Not supported")

class Field(input: Seq[String]):
  private val data: Array[Array[Tile]] =
    input.toArray.map:
      _.toCharArray.map:
        case 'S' => Start
        case '.' => Ground
        case other => Connector.from(other)

  lazy val height: Int = data.length
  lazy val width: Int = data(0).length

  lazy val loop: Loop =
    def findStart: Coordinates =
      val row = data.indexWhere(_.contains(Start))
      val col = data(row).indexWhere(_ == Start)
      Coordinates(row, col)

    @tailrec
    def populateLoop(current: Coordinates, inLoop: List[Coordinates] = List()): List[Coordinates] =
      inLoop match
        case Nil => populateLoop(current, List(current))
        case head :: tail =>
          val nextCoordinates: List[Coordinates] = (next(head), tail.headOption) match
            case (rawNext, Some(previousCoordinatesInLoop)) => rawNext.filterNot(_ == previousCoordinatesInLoop)
            case (rawNext, None) => rawNext

          nextCoordinates match
            case Nil => inLoop
            case onlyOne :: Nil if onlyOne == current => inLoop
            case onlyOne :: Nil => populateLoop(current, onlyOne +: inLoop)
            case _ => throw Exception(s"Not managed")

    Loop(populateLoop(findStart))

  private def valueAt(position: Coordinates): Tile = data(position.row)(position.col)

  private def next(from: Coordinates): List[Coordinates] =
    def manageStart: List[Coordinates] =
      given Field = this
      val toStartAt = Direction.values.map(currentDir => (from.move(currentDir), currentDir)).filter(_._1.isDefined).map:
        (coordinates, direction) => (coordinates, valueAt(coordinates), direction)
      .find:
        case (_, connector: Connector, direction) if connector.connects(direction.opposite) => true
        case _ => false
      .map(_._1)

      List(toStartAt.get)

    valueAt(from) match
      case Start => manageStart
      case Connector(firstDir, secondDir) => List(from.move(firstDir), from.move(secondDir))
      case _ => throw Exception("Not managed")

class Loop(points: List[Coordinates]):
  lazy val size: Int = points.size / 2

  lazy val internalArea: Int = area(0)

  lazy val globalArea: Int = area(1)

  private lazy val area: (Int, Int) =
    val borders = (points ::: points.take(2)).sliding(3).flatMap:
      case List(pointBefore, currentPoint, pointAfter) => guessBordersEdges(pointBefore, currentPoint, pointAfter)
      case _ => throw Exception(s"Not possible")
    .toVector.unzip

    val List(internalBorderArea, externalBorderArea) = borders.toList.map(computeArea).sorted

    (internalBorderArea, externalBorderArea)

  private def computeArea(edges: Vector[Coordinates]): Int =
    val nextOnes = edges.tail :+ edges.head
    val result = edges.zip(nextOnes).foldLeft(0):
      case (acc, (Coordinates(x1, y1),Coordinates(x2, y2))) => acc + (x2 * y1) - (x1 * y2)

    (result / 2).abs

  private def guessBordersEdges(pointBefore: Coordinates, currentPoint: Coordinates, pointAfter: Coordinates): Option[(Coordinates, Coordinates)] =
    (pointBefore comparedTo currentPoint, pointAfter comparedTo currentPoint) match
      case (Above, AtLeft) | (AtRight, Below) => Some((currentPoint.south.east, currentPoint))
      case (Above, AtRight) | (AtLeft, Below) => Some((currentPoint.east, currentPoint.south))
      case (Below, AtLeft) | (AtRight, Above) => Some((currentPoint.south, currentPoint.east))
      case (Below, AtRight) | (AtLeft, Above) => Some((currentPoint, currentPoint.south.east))
      case _ => None