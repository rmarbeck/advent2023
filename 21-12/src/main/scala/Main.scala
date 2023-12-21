import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._

import java.time.Duration
import java.time.Instant
// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching 21-12")
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

    val (result1, result2) = ("", "")
    val container = Container(lines)

    val container2 = container.homotethie(3)

    val iContainer = InfiniteContainer(container)

    println(container.emptyPlaces)
    println(container2.emptyPlaces)

    /*10 to 16 foreach : index =>
      println(s"Number of positions for ${index} = ${container.walkFromStart(index)}")
      val result = iContainer.walkFromStart(index)
      println(s"Number of positions for ${index} sur I = ${result} => ${(index+1)*(index+1) - result._1}")

    129 to 132 foreach: index =>
      println(s"Number of positions for ${index} = ${container.walkFromStart(index)}")
      val result = iContainer.walkFromStart(index)
      println(s"Number of positions for ${index} sur I = ${result}")*/

    1 to 1 foreach : index =>
      println(s"Number of positions for ${index} = ${container.walkFromStart(index)}")
      val result = iContainer.walkFromStart(index)
      //val result2 = iContainer.walkFromStart(index-131)
      //println(s"Number of positions for ${index} sur I = ${result}, ${result2} - ${result._1-result2._1}")


    val (part1, part2Plus) = iContainer.walkFromStart2(524).splitAt(131)
    val (part2, part3Plus) = part2Plus.splitAt(131)
    val (part3, part4Plus) = part3Plus.splitAt(131)
    val (part4, part5) = part4Plus.splitAt(131)

    part3.zip(part2).foreach:
      case (oneOfPart3, oneOfPart2) => println(s"${oneOfPart3 - oneOfPart2}")

    part4.zip(part3).foreach:
      case (oneOfPart4, oneOfPart3) => println(s"${oneOfPart4- oneOfPart3}")



    //println(container)
    //println("*******************")
    //println(container.findStart)

    //println(container.display(container.walkThrough(List(container.findStart), 1)))

    /*println(s"Number of positions = ${container.walkFromStart(13)}")
    println(s"Number of positions = ${container.walkFromStart(14)}")
    println(s"Number of positions = ${container.walkFromStart(15)}")

    println(s"Number of positions = ${container2.walkFromStart(23)}")
    println(container2.display(container2.walkThrough(List(container2.findStart), 23)))
    println(s"Number of positions = ${container2.walkFromStart(24)}")
    println(s"------------------------------------------")
    println(container2.display(container2.walkThrough(List(container2.findStart), 24)))
    println(s"------------------------------------------")

    println(s"Number of positions = ${container2.walkFromStart(43)}")
    println(container2.display(container2.walkThrough(List(container2.findStart), 43)))
    println(s"------------------------------------------")
    println(container2.display(container2.walkThrough(List(container2.findStart), 44)))
    println(s"Number of positions = ${container2.walkFromStart(44)}")
    println(s"Number of positions = ${container2.walkFromStart(45)}")
    println(s"Number of positions = ${container2.walkFromStart(46)}")*/


    println(s"------------------------------------------")

    /*println(s"Number of positions = ${iContainer.walkFromStart(10)}")
    println(s"Number of positions = ${iContainer.walkFromStart(50)}")
    println(s"Number of positions = ${iContainer.walkFromStart(100)}")
    println(s"Number of positions = ${iContainer.walkFromStart(500)}")*/
    /*println(s"Number of positions = ${iContainer.walkFromStart(107)}")
    println(s"Number of positions = ${iContainer.walkFromStart(108)}")
    println(s"Number of positions = ${iContainer.walkFromStart(109)}")*/

    //println(s"Number of positons = ${iContainer.walkFromStart(16)}")
    //println(s"Number of positions = ${iContainer.walkFromStart(256)}")

    //println(container.display(container.walkThrough(List(container.findStart), 6)))


    (s"${result1}", s"${result2}")

case class Position(row: Int, col: Int):
  def up = this.copy(row = row - 1)
  def down = this.copy(row = row + 1)
  def left = this.copy(col = col - 1)
  def right = this.copy(col = col + 1)

class Container(input: Seq[String]):
  val width = input(0).length
  val height: Int = input.length
  val data: Array[Array[Char]] = input.toArray.map(_.toArray)

  def emptyPlaces: Int = data.flatten.filter(_ == '.').length + 1

  def this(input: Seq[String], startPosition: Position) =
    this(input)
    data(startPosition.row)(startPosition.col) = 'S'

  def homotethie(times: Int): Container =
    val toConcatenate = input.map(_.map:
      case 'S' => '.'
      case value => value
    *times)
    val Position(innerStartRow, innerStartCol) = this.findStart
    val newStart = Position(innerStartRow + (times/2) * this.height, innerStartCol + (times/2) * this.width)
    Container((1 to times).flatMap(_ => toConcatenate).toList, newStart)

  def display(positions: List[Position]): String =
    val tempo = data.map(_.clone())
    positions.foreach:
      position => tempo(position.row)(position.col) = 'O'
    asString(tempo)

  def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  override def toString =
    asString(this.data)

  def walkFromStart(nSteps: Int): Long = walkThrough(List(findStart), nSteps).length

  def findStart: Position =
    val rowOfStart = data.indexWhere(_.contains('S'))
    Position(rowOfStart, data(rowOfStart).indexOf('S'))

  def walkThrough(from: List[Position], remainingSteps: Int): List[Position] =
    def findNext(position: Position): List[Position] =
      List(position.up, position.down, position.left, position.right).filter:
        case Position(row, col) => this.data.isDefinedAt(row) && this.data(row).isDefinedAt(col)
      .filter:
        case Position(row, col) => this.data(row)(col) != '#'

    remainingSteps match
      case 0 => from
      case _ =>
        val newFromPositions = from.flatMap(findNext(_)).distinct
        walkThrough(newFromPositions, remainingSteps - 1)

case class IStats(nbContainers: Long)

object IStats:
  def fromLimits(baseContainer: Container, limits: (Long, Long, Long, Long)) =
    val (maxRow, minRow, maxCol, minCol) = limits
    val (newHeight, newWidth) = (maxRow - minRow, maxCol - minCol)
    val minimumOfContainers = max(newHeight / baseContainer.height.toLong, newWidth / baseContainer.width.toLong)
    IStats(minimumOfContainers)

class InfiniteContainer(baseContainer: Container):
  var maxRow, minRow, maxCol, minCol = 0l
  def updateLimit(row: Long, col: Long): Unit =
    maxRow = max(maxRow, row)
    minRow = min(minRow, row)
    maxCol = max(maxCol, col)
    minCol = min(maxRow, col)

  def modulo(value: Long, max: Int): Int = value match
    case current if current % max < 0 => (current % max + max).toInt
    case current => (value % max).toInt
  def getValue(row: Long)(col: Long): Char =
    updateLimit(row, col)
    (modulo(row,baseContainer.height), modulo(col,baseContainer.width)) match
      case (newRow, newCol) if newRow != row.toInt || newCol != col.toInt => baseContainer.data(newRow)(newCol) match
        case 'S' => '.'
        case value => value
      case (newRow, newCol) => baseContainer.data(newRow)(newCol)

  def walkFromStart(nSteps: Int): (Long, IStats) = (walkThrough(List(baseContainer.findStart), nSteps).length, IStats.fromLimits(baseContainer, (maxRow, minRow, maxCol, minCol)))

  def walkFromStart2(nSteps: Int): Array[Int] = walkThrough2(List(baseContainer.findStart), nSteps, Array[Int]())

  def walkThrough(from: List[Position], remainingSteps: Int): List[Position] =
    def findNext(position: Position): List[Position] =
      List(position.up, position.down, position.left, position.right)
      .filter:
        case Position(row, col) => this.getValue(row)(col) != '#'

    remainingSteps match
      case 0 => from
      case _ =>
        val newFromPositions = from.flatMap(findNext(_)).distinct
        loggerAOCPart2.trace(s"${newFromPositions.length}")
        walkThrough(newFromPositions, remainingSteps - 1)

  def walkThrough2(from: List[Position], remainingSteps: Int, partialResult: Array[Int]): Array[Int] =
    def findNext(position: Position): List[Position] =
      List(position.up, position.down, position.left, position.right)
        .filter:
          case Position(row, col) => this.getValue(row)(col) != '#'

    remainingSteps match
      case 0 => partialResult
      case _ =>
        val newFromPositions = from.flatMap(findNext(_)).distinct
        loggerAOCPart2.trace(s"${newFromPositions.length}")
        walkThrough2(newFromPositions, remainingSteps - 1, partialResult :+ newFromPositions.length)