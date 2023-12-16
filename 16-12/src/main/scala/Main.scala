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

    //building box
    // Beam // Current Position => Box => ListOfBeams

    val container1 = Container(lines)
    //println(container1)
    pathLazyList(List((Beam(LeftToRight), container1.from(Position(0, 0, container1.heigh, container1.width)))), 0).last
    //println("<----------------------->")
    //println(container1)
    //println(container1.calc)

    val resultOf2 = (for row <- 0 until container1.heigh
        col <- 0 until container1.width
    yield
      def corner(colValue: Int): List[Dir] =
        colValue match
          case 0 => LeftToRight :: Nil
          case value if value == container1.width-1 => RightToLeft :: Nil
          case _ => Nil
      val dirs = (row, col) match
        case (0, valueOfCol) => UpToDown :: corner(valueOfCol)
        case (value, valueOfCol) if value == container1.heigh-1 => DownToUp :: corner(valueOfCol)
        case (_, valueOfCol) => corner(valueOfCol)
      (row, col, dirs)
      ).map {(row, col, currentDirs) =>
        currentDirs.map { currentDir =>
          val container = Container(lines)
          pathLazyList(List((Beam(currentDir), container.from(Position(row, col, container.heigh, container.width)))), 0).lastOption
          container.calc
        }.maxOption.getOrElse(0)
    }.max


    val beams = List(Beam(LeftToRight))
    var positions = List(Position(0, 0, 10, 10))


    val (result1, result2) = (s"${container1.calc}", s"$resultOf2")



    (s"${result1}", s"${result2}")

class Container(input: Seq[String]):
  val width = input(0).length
  val heigh = input.length
  val data: Array[Array[Box]] = Array.tabulate(heigh, width)((row, col) => Box('.', Position(row, col, heigh, width), this))
  val inputArray = input.toArray.map(_.toArray)
  for row <- 0 until heigh
      col <- 0 until width
  do
    inputArray(row)(col) match
      case '.' => ()
      case value => data(row)(col) = Box(value, Position(row, col, heigh, width), this)

  def nextBox(currentBox: Box, beam: Beam): Option[Box] =
    currentBox.positionInContainer.next(beam).map(position => data(position.row)(position.col))

  def from(position: Position): Box =
    data(position.row)(position.col)

  def calc: Int =
    data.flatten.foldLeft(0) {(acc, value) => value.isPowered match
      case true => acc + 1
      case false => acc + 0
    }

  override def toString =
    data.map(_.mkString(" ")).mkString("\n")

case class Position(row: Int, col: Int, maxRow: Int, maxCol: Int):
  def next(beam: Beam): Option[Position] =
    beam match
      case Beam(UpToDown) => row != maxRow-1 match
        case true => Some(this.copy(row = row+1))
        case false => None
      case Beam(DownToUp) => row != 0 match
        case true => Some(this.copy(row = row - 1))
        case false => None
      case Beam(LeftToRight) => col != maxCol-1 match
        case true => Some(this.copy(col = col + 1))
        case false => None
      case Beam(RightToLeft) => col != 0 match
        case true => Some(this.copy(col = col - 1))
        case false => None


case class Beam(from: Dir)

class Box(val typology: Char, val positionInContainer: Position, container: Container):
  var lightPassedUpward = false
  var lightPassedDownward = false
  var lightPassedRightward = false
  var lightPassedLeftward = false
  def passThrough(beam: Beam): List[(Beam, Box)] =
    val result = nextDirections(beam)
    beam.from match
      case UpToDown => lightPassedDownward = true
      case DownToUp => lightPassedUpward = true
      case LeftToRight => lightPassedRightward = true
      case RightToLeft => lightPassedLeftward = true

    result
  def isPowered: Boolean =
    lightPassedDownward || lightPassedLeftward || lightPassedRightward || lightPassedUpward

  def canTranverse(beam: Beam): Boolean =
    beam.from match
      case UpToDown if lightPassedDownward => false
      case DownToUp if lightPassedUpward => false
      case LeftToRight if lightPassedRightward => false
      case RightToLeft if lightPassedLeftward => false
      case _ => true

  def nextDirections(beam: Beam): List[(Beam, Box)] =
    (typology, beam, canTranverse(beam)) match
      case ('.', value, true) => singleNext(value)
      case ('/', value, true) => singleNext(Beam(value.from.throughSlash))
      case ('\\', value, true) => singleNext(Beam(value.from.throughAntiSlash))
      case ('-', value, true) => value.from match
        case UpToDown | DownToUp => List(next(Beam(LeftToRight)), next(Beam(RightToLeft))).filterNot(_.isEmpty).map(_.get)
        case value => singleNext(Beam(value))
      case ('|', value, true) => value.from match
        case LeftToRight | RightToLeft => List(next(Beam(UpToDown)), next(Beam(DownToUp))).filterNot(_.isEmpty).map(_.get)
        case value => singleNext(Beam(value))
      case _ => List()

  def singleNext(beam: Beam): List[(Beam, Box)] =
    next(beam).map(List(_)).getOrElse(List())

  def next(beam: Beam): Option[(Beam, Box)] =
    container.nextBox(this, beam) match
      case Some(box) => Some(beam, box)
      case None => None


  override def toString(): String =
    s"${
      isPowered match
        case true => '#'
        case false => typology
    }"

def pathLazyList(beams: List[(Beam, Box)], step: Int): LazyList[Int] =
  val newBeams = beams.flatMap { (currentBeam, currentBox) =>
    currentBox.passThrough(currentBeam)
  }
  if (newBeams.length == 0) LazyList.empty
  else LazyList.cons(step, pathLazyList(newBeams, step + 1))

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
