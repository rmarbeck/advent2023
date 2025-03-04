import RelativePosition.*
import Direction.*

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val edges = inputLines.foldLeft((Path.start, Path.start)):
      case ((pathPart1, pathPart2), s"${DirectionExt(direction)} ${StepsExt(steps)} ($color)") =>
        (pathPart1.moveForward(direction, steps), pathPart2.moveForward(color))
      case _ => throw Exception("Error in parsing")

    val List(result1, result2) = edges.toList.map(Loop.from(_).globalArea)

    (s"$result1", s"$result2")

case class Path private (positions: List[Position]):
  def moveForward(direction: Direction, steps: Int): Path =
    Path((positions.head nextBy(direction, steps)) +: positions)

  def moveForward(color: String): Path =
    Path((positions.head nextBy color) +: positions)

  def allButHead: List[Position] = positions.tail

object DirectionExt:
  def unapply(str: String): Option[Direction] =
    if Direction.isDefinedFrom(str) then
      Some(Direction.fromString(str))
    else
      None

object StepsExt:
  def unapply(str: String): Option[Int] = str.toIntOption

object Path:
  def start: Path = Path(List(Position(0, 0)))

object Loop:
  def from(path: Path): Loop = Loop(path.allButHead)

case class Loop(points: List[Position]):
  lazy val internalArea: Long = area(0)

  lazy val globalArea: Long = area(1)

  private lazy val area: (Long, Long) =
    val borders = (points ::: points.take(2)).sliding(3).flatMap:
      case List(pointBefore, currentPoint, pointAfter) => guessBordersEdges(pointBefore, currentPoint, pointAfter)
      case _ => throw Exception(s"Not possible")
    .toList.unzip

    val List(internalBorderArea, externalBorderArea) = borders.toList.map(computeArea).sorted

    (internalBorderArea, externalBorderArea)

  private def computeArea(edges: List[Position]): Long =
    val nextOnes = edges.tail :+ edges.head
    val result = edges.zip(nextOnes).foldLeft(0L):
      case (acc, bothPoints) =>
        val (x1, y1, x2, y2) = (bothPoints(0).row, bothPoints(0).col, bothPoints(1).row, bothPoints(1).col)
        acc + (x2 * y1) - (x1 * y2)

    math.abs(result / 2)

  private def guessBordersEdges(pointBefore: Position, currentPoint: Position, pointAfter: Position): Option[(Position, Position)] =
    (pointBefore comparedTo currentPoint, pointAfter comparedTo currentPoint) match
      case (Above, AtLeft) | (AtRight, Below) => Some((currentPoint.south.east, currentPoint))
      case (Above, AtRight) | (AtLeft, Below) => Some((currentPoint.east, currentPoint.south))
      case (Below, AtLeft) | (AtRight, Above) => Some((currentPoint.south, currentPoint.east))
      case (Below, AtRight) | (AtLeft, Above) => Some((currentPoint, currentPoint.south.east))
      case _ => None

enum Direction:
  case Up, Right, Down, Left

object Direction:
  def isDefinedFrom(string: String): Boolean =
    string.length match
      case 1 => from(string.head).isDefined
      case _ => false

  private def from(char: Char): Option[Direction] =
    char match
      case 'U' | '3' => Some(Up)
      case 'R' | '0' => Some(Right)
      case 'D' | '1' => Some(Down)
      case 'L' | '2' => Some(Left)
      case _ => None

  def fromString(string: String): Direction =
    isDefinedFrom(string) match
      case true => from(string.head).get
      case false => throw Exception("Not supported")

enum RelativePosition:
  case Above, Below, AtRight, AtLeft


case class Position(row: Long, col: Long):
  lazy val south: Position = next(Down, 1)
  lazy val east: Position = next(Right, 1)

  private def next(direction: Direction, steps: Long): Position =
    direction match
      case Up => this.copy(row = row - steps)
      case Down => this.copy(row = row + steps)
      case Left => this.copy(col = col - steps)
      case Right => this.copy(col = col + steps)

  def nextBy(direction: Direction, steps: Int): Position = next(direction, steps)

  def nextBy(rawInHexadecimal: String): Position =
    next.tupled(rawInHexadecimal.parsePart2)

  def comparedTo(other: Position): RelativePosition =
    (other.row - row, other.col - col) match
      case (value , 0) if value < 0 => Below
      case (value , 0) if value > 0 => Above
      case (0, value) if value > 0 => AtLeft
      case (0, value) if value < 0 => AtRight
      case _ => throw Exception(s"Not supported $other")

extension (string: String)
  def parsePart2: (Direction, Long) =
    string match
      case s"#$hexadecimal" =>
        val (first, last) = hexadecimal.splitAt(5)
        val steps = first.foldLeft(0L):
          (acc, char) => acc * 16 + char.asDigit
        (Direction.fromString(last), steps)
      case _ => throw Exception("Not in expected format")

