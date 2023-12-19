import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

import Dir._
import Turn._

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching X-12")
  val startTime = Instant.now()

  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")
  println(s"Computing time is ${Duration.between(startTime, Instant.now()).toMillis}ms")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq

    val maze1 = Maze(lines)

    println(maze1)
    println("**************")

    val maze = lines.map(_.toCharArray).toArray

    val path = Path.from(getListOfPoints(maze))

    println(path.length)

    println(maze1.display(path))

    val path2 = Path(Point(0,0), List(Directive(LeftToRight, 5), Directive(UpToDown, 5), Directive(RightToLeft, 5), Directive(DownToUp, 5)))

    /*println(s" path2 ----------> ${path2.getArea} and length = ${path2.length}")
    path2.getBorders.map(println)*/
    println(s" ----------> ${path.getArea} and length = ${path.length} => ${path.getArea - path.length}")
    //path.points.map(println)

    //println(s"maze size : ${maze(0).length} and ${maze.length}")

    val pipes = Seq("|", "-", "7", "F", "L", "J", "S", ".")

    val (result1, result2) = (s"${path.length/2}", s"${path.getArea - path.length}")

    (s"${result1}", s"${result2}")

enum Direction:
  case North, East, South, West

class Maze(input: Seq[String]):
  val width = input(0).length
  val height: Int = input.length
  val data: Array[Array[Char]] = input.map(_.toCharArray).toArray

  def displayFull(path: Path): String =
    display(path.getBorders)

  def display(path: Path): String =
    display(path.getEdges)

  def display(points: List[Point]): String =
    val tempo = data.map(_.map(_.toString.head).clone())
    points.foreach:
      point => tempo(point.lineNum.toInt)(point.colNum.toInt) = '#'
    asString(tempo)

  def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  override def toString =
    asString(data.map(_.map(_.toString.head)))

def getListOfPoints(maze: Array[Array[Char]]): List[Point] =
  resolveMaze(maze).map(value => Point(value._1.toLong, value._2.toLong)).toList

def resolveMaze(maze: Array[Array[Char]]): List[(Int, Int)] =
  def findStart: (Int, Int) =
    var i, j = 0
    var found = false
    while (found == false) {
      if (maze(i)(j) == 'S') {
        found = true
      } else {
        if (j == maze(0).length-1) {
          j = 0
          i = i + 1
        } else {
          j = j +1
        }
      }
    }

    //println(s"i = $i, j = $j")
    (i, j)


  def progressiveSnake(current: (Int, Int), previousDir: Option[Direction]): LazyList[((Int, Int), Char)] =
    def nextElement = findNext(current, previousDir)

    if (nextElement.isEmpty) LazyList.empty
    else LazyList.cons((current, maze(current._1)(current._2)), progressiveSnake(nextElement.get._1, nextElement.get._2))

  def findNext(current: (Int, Int), previousDir: Option[Direction]): Option[((Int, Int), Option[Direction])] =
    import Direction._
    val (currentRow, currentColumn) = current
    val currentLetter = maze(current._1)(current._2)

    def isPossible(direction: Direction): Boolean =
      direction match
        case North if currentRow > 1 => maze(currentRow-1)(currentColumn) match
          case '|'|'7'|'F' => true
          case _ => false
        case East if currentColumn < maze(0).length => maze(currentRow)(currentColumn+1) match
          case '-' | 'L' | 'F' => true
          case _ => false
        case South if currentRow < maze.length => maze(currentRow+1)(currentColumn) match
          case 'J' | '|' | 'L' => true
          case _ => false
        case West if currentColumn > 1 => maze(currentRow)(currentColumn-1) match
          case '-' | 'J' | '7' => true
          case _ => false
        case _ => false

    def findIN(directions: Seq[Direction]): Seq[Direction] =
      directions.filter(isPossible(_))

    def isForbidden(nextDirection: Direction): Boolean =
      previousDir match
        case None => false
        case Some(previousDirection) => previousDirection match
          case North if nextDirection == South => true
          case East if nextDirection == West => true
          case South if nextDirection == North => true
          case West if nextDirection == East => true
          case _ => false

    def output(direction: Direction): ((Int, Int), Option[Direction]) =
      direction match
        case North => ((currentRow-1, currentColumn), Some(North))
        case East => ((currentRow, currentColumn+1), Some(East))
        case South => ((currentRow + 1, currentColumn), Some(South))
        case West => ((currentRow, currentColumn-1), Some(West))

    val nextDir = currentLetter match {
      case 'S' if previousDir.isDefined => None
      case value => value match {
        case 'S' => Some(findIN(Seq(North, East, South, West)))
        case '|' => Some(Seq(North, South))
        case '-' => Some(Seq(West, East))
        case '7' => Some(Seq(South, West))
        case 'F' => Some(Seq(East, South))
        case 'L' => Some(Seq(North, East))
        case 'J' => Some(Seq(North, West))
      }
    }

    nextDir match
      case None => None
      case Some(value: Seq[Direction]) => Some(output(value.filterNot(isForbidden(_)).head))

  val partOfTheLoop = progressiveSnake(findStart, None).filterNot(value => value._2 == '-' || value._2 == '|').map(current => (current._1._1,current._1._2))

  //val lengthOfQueue = progressiveSnake(findStart, None).length / 2


  partOfTheLoop.toList


case class Directive(direction: Dir, step: Long)

object Directive:
  def fromLetter(letter: Char, steps: Long): Directive =
    val direction = letter match
      case 'R' => LeftToRight
      case 'D' => UpToDown
      case 'L' => RightToLeft
      case 'U' => DownToUp
    Directive(direction, steps)

case class Point(lineNum: Long, colNum: Long):
  lazy val upLeft = this.copy()
  lazy val upRight = this.copy(colNum = colNum+1)
  lazy val downLeft = this.copy(lineNum = lineNum+1)
  lazy val downRight = Point(lineNum+1, colNum+1)

class Path(start: Point, dirs: List[Directive]):
  lazy val points: List[Point] = getEdges
  lazy val length: Int = getBorders.distinct.length
  def getArea: Long =
    def getCoordsOfEdgesBorders: List[(Point, Point)] =
      def guessTurn(first: Point, second: Point, third: Point): Option[Turn] =
        val result = (first.lineNum, third.lineNum, first.colNum, third.colNum) match
          case (line1, line3, col1, col3) if line1 == line3 || col1 == col3 => None
          case (line1, line3, col1, col3) if col1 > col3 && line1 < line3 =>
            second.lineNum match
              case value if value == line1 => Some(RightToLeftGoDown)
              case _ => Some(UpToDownGoLeft)
          case (line1, line3, col1, col3) if col1 > col3 && line1 > line3 =>
            second.lineNum match
              case value if value == line1 => Some(RightToLeftGoUp)
              case _ => Some(DownToUpGoLeft)
          case (line1, line3, col1, col3) if col1 < col3 && line1 < line3 =>
            second.lineNum match
              case value if value == line1 => Some(LeftToRightGoDown)
              case _ => Some(UpToDownGoRight)
          case (line1, line3, col1, col3) if col1 < col3 && line1 > line3 =>
            second.lineNum match
              case value if value == line1 => Some(LeftToRightGoUp)
              case _ => Some(DownToUpGoRight)

        //println(s"ligne1 ${first.lineNum} ligne3 ${third.lineNum}  < - > col1 ${first.colNum}  col3 ${third.colNum} => $result ")
        result

      val closedEdges = getEdges.dropRight(1) ::: getEdges.take(2)
      //closedEdges.sliding(3, 1).foreach(current=> println(s"----- > ${current}"))
      closedEdges.sliding(3, 1).map:
        case List(first, second, third) => guessTurn(first, second, third) match
          case None => None
          case Some(value) => value match
            case LeftToRightGoUp | DownToUpGoRight => Some((second.upLeft, second.downRight))
            case LeftToRightGoDown | UpToDownGoRight => Some((second.upRight, second.downLeft))
            case RightToLeftGoUp | DownToUpGoLeft => Some((second.downLeft, second.upRight))
            case RightToLeftGoDown | UpToDownGoLeft => Some((second.downRight, second.upLeft))
        case _ => None
      .filterNot(_.isEmpty)
      .map(_.get)
      .toList

    points.length match
      case value if value >= 3 =>
        val firstBorder = getCoordsOfEdgesBorders.unzip._1
        val secondBorder = getCoordsOfEdgesBorders.unzip._2
        max(calcArea(firstBorder), calcArea(secondBorder))
      case _ => 0l

  def getBorders: List[Point] =
    (points).zip(points.tail :+ points.head).flatMap((point1, point2) =>
      (point1.lineNum, point2.lineNum, point1.colNum, point2.colNum) match
        case (x1, x2, y1, y2) if x1 == x2 => (y1 - y2 > 0) match
          case false => y1 to y2 map (coord => Point(x1, coord))
          case _ => y2 to y1 map (coord => Point(x1, coord))
        case (x1, x2, y1, y2) if y1 == y2 => (x1 - x2 > 0) match
          case false => x1 to x2 map (coord => Point(coord, y1))
          case _ => x2 to x1 map (coord => Point(coord, y1))
    )

  def getEdges: List[Point] =
    def nextEdges(currentPoint: Point, dirs: List[Directive]):List[Point] =
      dirs match
        case Nil => Nil
        case value :: Nil => nextEdge(currentPoint, value.direction, value.step) :: Nil
        case head :: tail =>
          val nextPointCalculated = nextEdge(currentPoint, head.direction, head.step)
          nextPointCalculated :: nextEdges(nextPointCalculated, tail)

    def nextEdge(currentPoint: Point, dir: Dir, steps: Long): Point =
      dir match
        case UpToDown => currentPoint.copy(lineNum = currentPoint.lineNum+steps)
        case DownToUp => currentPoint.copy(lineNum = currentPoint.lineNum-steps)
        case LeftToRight => currentPoint.copy(colNum = currentPoint.colNum+steps)
        case RightToLeft => currentPoint.copy(colNum = currentPoint.colNum-steps)

    val closedDirs = dirs :+ dirs.head
    nextEdges(start, closedDirs)

object Path:
  def from(listOfPoints: List[Point]): Path =
    //listOfPoints.map(println)
    val directives = listOfPoints.zip(listOfPoints.tail).map:
      case (point1, point2) if point1.lineNum == point2.lineNum =>
        (point1.colNum - point2.colNum) > 0 match
          case true => Directive(RightToLeft, point1.colNum - point2.colNum)
          case false => Directive(LeftToRight, point2.colNum - point1.colNum)
      case (point1, point2) if point1.colNum == point2.colNum =>
        (point1.lineNum - point2.lineNum) > 0 match
          case true => Directive(DownToUp, point1.lineNum - point2.lineNum)
          case false => Directive(UpToDown, point2.lineNum - point1.lineNum)
    Path(listOfPoints.head, directives)

def calcArea(points: List[Point]): Long =
  val nextPoints = points.tail :+ points.head
  val result = points.zip(nextPoints).foldLeft(0l) {(acc, newValues) =>
    val (x1, y1, x2, y2) = (newValues._1.lineNum, newValues._1.colNum, newValues._2.lineNum, newValues._2.colNum)
    acc + (x2*y1) - (x1*y2)
  }
  result / 2

enum Turn:
  case LeftToRightGoUp, RightToLeftGoUp, LeftToRightGoDown, RightToLeftGoDown, UpToDownGoLeft, UpToDownGoRight, DownToUpGoLeft, DownToUpGoRight

enum Dir:
  case UpToDown, DownToUp, LeftToRight, RightToLeft