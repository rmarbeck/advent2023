import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._

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


    val testDirs = List(Directive(LeftToRight, 4), Directive(UpToDown, 1), Directive(LeftToRight, 2), Directive(DownToUp, 1), Directive(LeftToRight, 4), Directive(UpToDown, 10), Directive(RightToLeft, 10), Directive(DownToUp, 10))
    val testDirs2 = List(Directive(LeftToRight, 10), Directive(UpToDown, 10), Directive(RightToLeft, 10), Directive(DownToUp, 10))
    val path1 = Path(Point(0, 0), testDirs2)
    println(path1.getBorders.length)
    println(s"Area = ${path1.getArea}")
    println(Container.fromDims(path1.height, path1.width).display(path1))

    val dirs = lines.map :
      case s"$dir $step ($color)" => Directive.fromLetter(dir.toCharArray.head, step.toInt)

    val path = Path(Point(0, 0), dirs.toList)

    println(path.getBorders.length)
    println(s"Area = ${path.getArea}")
    //println(Container.fromDims(path.height, path.width).display(path))

    val (result1, result2) = (s"${path.getArea}", "")

    (s"${result1}", s"${result2}")

class Container(input: Seq[String]):
  val width = input(0).length
  val height: Int = input.length
  val data: Array[Array[Int]] = input.toArray.map(_.toArray.map(_.asDigit))

  def displayFull(path: Path): String =
    display(path.getBorders)

  def display(path: Path): String =
    display(path.getEdges)

  def display(points: List[Point]): String =
    val tempo = data.map(_.map(_.toString.head).clone())
    points.foreach:
      point => tempo(point.lineNum)(point.colNum) = '#'
    asString(tempo)

  def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  override def toString =
    asString(data.map(_.map(_.toString.head)))

object Container:
  def fromDims(height: Int, length: Int): Container =
    val lines = "." * length
    Container(0 until height map (_ => lines))


case class Directive(direction: Dir, step: Int)

object Directive:
  def fromLetter(letter: Char, steps: Int): Directive =
    val direction = letter match
      case 'R' => LeftToRight
      case 'D' => UpToDown
      case 'L' => RightToLeft
      case 'U' => DownToUp
    Directive(direction, steps)

case class Point(lineNum: Int, colNum: Int):
  lazy val upLeft = this.copy()
  lazy val upRight = this.copy(colNum = colNum+1)
  lazy val downLeft = this.copy(lineNum = lineNum+1)
  lazy val downRight = Point(lineNum+1, colNum+1)

class Path(start: Point, dirs: List[Directive]):
  lazy val points: List[Point] = getEdges
  lazy val height: Int = points.map(_.lineNum).max + 1 - points.map(_.lineNum).min
  lazy val width: Int = points.map(_.colNum).max + 1 - points.map(_.colNum).min
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
          case None => println("should not happen"); (second, second.downRight)
          case Some(value) => value match
            case LeftToRightGoUp | DownToUpGoRight => (second.upLeft, second.downRight)
            case LeftToRightGoDown | UpToDownGoRight => (second.upRight, second.downLeft)
            case RightToLeftGoUp | DownToUpGoLeft => (second.downLeft, second.upRight)
            case RightToLeftGoDown | UpToDownGoLeft => (second.downRight, second.upLeft)
      .toList

    println(s"number of points = ${points.length}")
    points.length match
      case value if value >= 3 =>
        val firstBorder = getCoordsOfEdgesBorders.unzip._1
        firstBorder.map(println)
        println(s"=> ${calcArea(firstBorder)}")
        val secondBorder = getCoordsOfEdgesBorders.unzip._2
        println("************************")
        secondBorder.map(println)
        println(s"=> ${calcArea(secondBorder)}")
        max(calcArea(firstBorder), calcArea(secondBorder))
      case _ => 0l


  def getBorders: List[Point] =
    points.zip(points.tail).flatMap((point1, point2) =>
      (point1.lineNum, point2.lineNum, point1.colNum, point2.colNum) match
        case (x1, x2, y1, y2) if x1 == x2 => (y1 - y2 > 0) match
            case false => y1 until y2 map(coord => Point(x1, coord))
            case _ => y2 until y1 map(coord => Point(x1, coord))
        case (x1, x2, y1, y2) if y1 == y2 => (x1 - x2 > 0) match
            case false => x1 until x2 map(coord => Point(coord, y1))
            case _ => x2 until x1 map(coord => Point(coord, y1))
    )

  def getEdges: List[Point] =
    def nextEdges(currentPoint: Point, dirs: List[Directive]):List[Point] =
      dirs match
        case Nil => Nil
        case value :: Nil => nextEdge(currentPoint, value.direction, value.step) :: Nil
        case head :: tail =>
          val nextPointCalculated = nextEdge(currentPoint, head.direction, head.step)
          nextPointCalculated :: nextEdges(nextPointCalculated, tail)

    def nextEdge(currentPoint: Point, dir: Dir, steps: Int): Point =
      dir match
        case UpToDown => currentPoint.copy(lineNum = currentPoint.lineNum+steps)
        case DownToUp => currentPoint.copy(lineNum = currentPoint.lineNum-steps)
        case LeftToRight => currentPoint.copy(colNum = currentPoint.colNum+steps)
        case RightToLeft => currentPoint.copy(colNum = currentPoint.colNum-steps)

    val closedDirs = dirs :+ dirs.head
    nextEdges(start, closedDirs)


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
  def throughSlash: Dir =
    this match
      case UpToDown => RightToLeft
      case DownToUp => LeftToRight
      case LeftToRight => DownToUp
      case RightToLeft => UpToDown

  def throughAntiSlash: Dir =
    this match
      case UpToDown => LeftToRight
      case DownToUp => RightToLeft
      case LeftToRight => UpToDown
      case RightToLeft => DownToUp