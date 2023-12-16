import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._

import scala.collection.parallel.*
import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

import java.time.Duration
import java.time.Instant
// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

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
    pathLazyList(List((Beam(Left2Right()), container1.from(Position(0, 0, container1.heigh, container1.width)))), 0).last
    //println("<----------------------->")
    //println(container1)
    //println(container1.calc)

    val resultOf2 = (for row <- 0 until container1.heigh
        col <- 0 until container1.width
    yield
      def fromBeam(beam: List[Beam]): Int =
        beam.map { currentBeam =>
          val container = Container(lines)
          pathLazyList(List((currentBeam, container.from(Position(row, col, container.heigh, container.width)))), 0).lastOption
          container.calc
        }.max
      (row, col) match
        case (0, value) =>
          value match
            case 0 => fromBeam(List(Beam(Up2Down()), Beam(Left2Right())))
            case colValue if colValue == container1.width-1 => fromBeam(List(Beam(Up2Down()), Beam(Right2Left())))
            case _ => fromBeam(List(Beam(Up2Down())))
        case (valueRow, valueCol) if valueRow == container1.heigh-1 =>
          valueCol match
            case 0 => fromBeam(List(Beam(Down2Up()), Beam(Left2Right())))
            case colValue if colValue == container1.width-1 => fromBeam(List(Beam(Down2Up()), Beam(Right2Left())))
            case _ => fromBeam(List(Beam(Down2Up())))
        case (_, 0) =>
          fromBeam(List(Beam(Left2Right())))
        case (_, value) if value == container1.width-1 =>
          fromBeam(List(Beam(Right2Left())))
        case _ => 0
    ).max


    val beams = List(Beam(Left2Right()))
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
      case Beam(Up2Down()) => row != maxRow-1 match
        case true => Some(this.copy(row = row+1))
        case false => None
      case Beam(Down2Up()) => row != 0 match
        case true => Some(this.copy(row = row - 1))
        case false => None
      case Beam(Left2Right()) => col != maxCol-1 match
        case true => Some(this.copy(col = col + 1))
        case false => None
      case Beam(Right2Left()) => col != 0 match
        case true => Some(this.copy(col = col - 1))
        case false => None


case class Beam(from: Direction)

sealed trait Direction(val degrees: Int) :
  def throughSlash: Direction
  def throughAntiSlash: Direction

case class Up2Down() extends Direction(-90) :
  override def throughSlash: Direction = Right2Left()
  override def throughAntiSlash: Direction = Left2Right()
case class Down2Up() extends Direction(90) :
  override def throughSlash: Direction = Left2Right()
  override def throughAntiSlash: Direction = Right2Left()
case class Left2Right() extends Direction(180) :
  override def throughSlash: Direction = Down2Up()
  override def throughAntiSlash: Direction = Up2Down()
case class Right2Left() extends Direction(0) :
  override def throughSlash: Direction = Up2Down()
  override def throughAntiSlash: Direction = Down2Up()

class Box(val typology: Char, val positionInContainer: Position, container: Container):
  var lightPassedUpward = false
  var lightPassedDownward = false
  var lightPassedRightward = false
  var lightPassedLeftward = false
  def passThrough(beam: Beam): List[(Beam, Box)] =
    val result = nextDirections(beam)
    beam.from match
      case Up2Down() => lightPassedDownward = true
      case Down2Up() => lightPassedUpward = true
      case Left2Right() => lightPassedRightward = true
      case Right2Left() => lightPassedLeftward = true

    result
  def isPowered: Boolean =
    lightPassedDownward || lightPassedLeftward || lightPassedRightward || lightPassedUpward

  def canTranverse(beam: Beam): Boolean =
    beam.from match
      case Up2Down() if lightPassedDownward => false
      case Down2Up() if lightPassedUpward => false
      case Left2Right() if lightPassedRightward => false
      case Right2Left() if lightPassedLeftward => false
      case _ => true

  def nextDirections(beam: Beam): List[(Beam, Box)] =
    (typology, beam, canTranverse(beam)) match
      case ('.', value, true) => singleNext(value)
      case ('/', value, true) => singleNext(Beam(value.from.throughSlash))
      case ('\\', value, true) => singleNext(Beam(value.from.throughAntiSlash))
      case ('-', value, true) => value.from match
        case Up2Down() | Down2Up() => List(next(Beam(Left2Right())), next(Beam(Right2Left()))).filterNot(_.isEmpty).map(_.get)
        case value => singleNext(Beam(value))
      case ('|', value, true) => value.from match
        case Left2Right() | Right2Left() => List(next(Beam(Up2Down())), next(Beam(Down2Up()))).filterNot(_.isEmpty).map(_.get)
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