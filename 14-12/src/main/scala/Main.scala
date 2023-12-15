import Direction.North
import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
import javax.print.attribute.standard.MediaSize.Other
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

enum Direction :
  case North, West, East, South
  def nextDir =
    this match
      case North => West
      case West => South
      case South => East
      case East => North

  def leapsCount(other: Direction) =
    (this, other) match
      case (value1, value2) if value1 == value2 => 0
      case (North, South) => 2
      case (North, East) => 3
      case (West, East) => 2
      case (West, North) => 3
      case (East, West) => 2
      case (East, South) => 3
      case (South, North) => 2
      case (South, West) => 3
      case _ => 1

class Support(input: Seq[String], val direction: Direction = North):
  import Direction._
  var content: Array[Array[Char]] = input.toArray.map(_.toArray)

  def this(from: Array[Array[Char]], direction: Direction) = this(from.map(_.mkString).toList, direction)

  def width: Int = content(0).length

  def height: Int = content.length

  def tilt(direction: Direction): Support =
    direction match
      case value => moveAllUp(this.swap(value)).swap(this.direction)

  def turn: Support =
    turnClockWise

  def turnAntiClockWise: Support =
    transpose(this)

  def turnClockWise: Support =
    transpose(Support(this.content.reverse, this.direction))

  def tiltAndTurn(times: Int): Support =
    (1 to times).foldLeft(this) {(acc, newvalue) =>
      moveAllUp(acc).turn
    }

  def tiltAndTurnFull(times: Int): Support =
    (1 to times).foldLeft(this) { (acc, newvalue) =>
      val result = acc.tiltAndTurn(4)
      //println(s"\u001b[2J ${result.calc}")
      result
    }

  def transpose(support: Support): Support =
    val newArray = support.content.map(_.clone())
    Support(newArray.transpose.map(_.mkString), support.direction.nextDir)

  def swap(direction: Direction): Support =
    val newArray = this.content.map(_.clone())
    val result = direction match
      case currentDirection if this.direction == currentDirection => Support(newArray, currentDirection)
      case North if this.direction == East => transpose(this)
      case West if this.direction == North => transpose(this)
      case South if this.direction == West => transpose(this)
      case East if this.direction == South => transpose(this)
      case value =>
        this.direction.leapsCount(value) match
          case 2 => println("there") ; transpose(transpose(this))
          case 3 => println("here") ; transpose(transpose(transpose(this)))
          case 1 => println("should not happen") ;transpose(this)
          case 0 => println("should not happen") ;this

    result

  def calc: Long =
    (for j <- 0 until this.width
         i <- 0 until this.height
    yield
      this.content(i)(j) match
        case 'O' => this.height - i
        case _ =>  0
    ).sum

  def moveAllUp(support: Support): Support =
    val tempArray = support.content.map(_.clone())
    val newArray = support.content.map(_.clone())
    for j <- 0 until support.width
        i <- support.height-1 to 0 by -1
    do
      loggerAOC.trace(s"entering $i, $j => ${tempArray(i)(j)}");
      newArray(i)(j) = tempArray(i)(j) match
        case '.' => loggerAOC.trace(s". en $i, $j"); '.'
        case value if value == 'O' ||  value == 'x' => loggerAOC.trace(s"$value en $i, $j"); isThereAPlaceAbove(tempArray, j, i) match
          case Some(value) =>
            loggerAOC.trace(s"putting x in ${i-value-1} $j and putting . in $i, $j")
            tempArray(i-value-1)(j) = 'x'
            '.'
          case None => loggerAOC.trace(s"putting O in $i, $j"); 'O'
        case '#' => loggerAOC.trace(s"# en $i, $j"); '#'
    Support(newArray, support.direction)


  def isThereAPlaceAbove(values: Array[Array[Char]], column: Int, line: Int): Option[Int] =
    val blocksBeforeRock =
      (for i <- line-1 to 0 by -1
      yield values(i)(column)).span(_ != '#')._1

    val result = blocksBeforeRock.zipWithIndex.find(_._1 == '.').map((character, index) => index).headOption.orElse(None)
    loggerAOC.trace(s"$blocksBeforeRock => ${result}")
    result

  override def toString: String =
    println(s"<->  ${this.direction}")
    this.direction match
      case North => content.map(_.mkString("")).mkString("\n")
      case _ => this.turn.toString

  def toStringInPlace: String =
    println(s"<->  ${this.direction}")
    content.map(_.mkString("")).mkString("\n")

object Solver:
  import Direction._
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq

    val asRows = lines.transpose.map(_.mkString)

    /*println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 0).getValueOf(100)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 1).getValueOf(100)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 2).getValueOf(100)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 3).getValueOf(100)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 4).getValueOf(100)}")

    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 0).getValueOf(101)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 1).getValueOf(101)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 2).getValueOf(101)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 3).getValueOf(101)}")
    println(s" => ${Cycle[Char](List('a', 'b', 'c', 'd', 'e'), 4).getValueOf(101)}")*/


    println("**************")
    //println(Support(lines, North).tiltAndTurnFull(3))
    //println(Support(lines, North).tiltAndTurnFull(1000).calc)
    //println(Support(lines, North).tiltAndTurnFull(2000).calc)
    /*println(s" 1260 => ${Support(lines, North).tiltAndTurnFull(1260).calc}")
    println(s" 1250 => ${Support(lines, North).tiltAndTurnFull(1250).calc}")
    println(s" 1206 => ${Support(lines, North).tiltAndTurnFull(1206).calc}")*/
    val current = Support(lines, North)
    var tempo = current
    println("--------------1")
    /*for i <- 0 to 100
    do
      println(s"$i => ${tempo.calc}")
      tempo = tempo.tiltAndTurnFull(1)*/

    println {
      lazyList(Support(lines, North)).last match
        case None => "not found"
        case Some(cycle) => cycle.getValueOf(1000000000)
    }


    /*println(Support(lines, North).tiltAndTurnFull(1))
    println("--------------2")
    println(Support(lines, North).tiltAndTurnFull(100))*/
    /*println(Support(lines, North).tilt(West))
    println("--------------2")
    println(Support(lines, North).tilt(West).tilt(South))
    println("--------------3")
    println(Support(lines, North).tilt(West).tilt(South).tilt(East))
    println("--------------4")
    println(Support(lines, North).tilt(West).tilt(South).tilt(East).tilt(North))
    println("**************")*/

    /*val result = asRows.map { currentLine =>
      val length = currentLine.length
      currentLine.zipWithIndex.foldLeft((0,length)) { (acc, newValue) =>
        newValue match
          case ('.', _) => acc
          case ('O', _) => (acc._1 + acc._2, acc._2 - 1)
          case ('#', currentIndex) => (acc._1, (length - (currentIndex+1) ))
      }._1
    }.sum*/

    val (result1, result2) = (s"", "")



    (s"${result1}", s"${result2}")

def lazyList(support: Support): LazyList[Option[Cycle[Long]]] =
  loadLazyList(support, List(0l))

def loadLazyList(support: Support, previous: List[Long]): LazyList[Option[Cycle[Long]]] =
  val tilted = support.tiltAndTurnFull(1)
  val computed = tilted.calc
  val newList = previous :+ computed
  val cycle = findCycle(previous)
  if (cycle.isDefined) LazyList.empty
  else LazyList.cons(findCycle(newList), loadLazyList(tilted, newList))

def findCycle(toAnalyse: List[Long]): Option[Cycle[Long]] =
  toAnalyse.length match
    case value if value < 10 => None
    case _ =>
      val head :: tail = toAnalyse.reverse
      val cycle = tail.zipWithIndex.filter((value, index) => value == head).map((value, index) => index+1).take(5) match
        case value if value.length < 5 => None
        case first :: second :: _ :: _ :: last :: List() =>
          val sizeOfFirst = second - first
          val sizeOfSecond = (last - second) / 3
          val firstList = tail.drop(first).take(second - first)
          val secondList = tail.drop(second).take(second - first)
          val lastList = tail.drop(last).take(second - first)
          sizeOfSecond == sizeOfFirst match
            case true =>
              firstList match
                case valueOfFirstPart if valueOfFirstPart == lastList => Some(second - first)
                case _ => None
            case false => None
        case _ => None

      cycle match
        case None => None
        case Some(0) => None
        case Some(value) =>
          val firstIndexOfResultingList = (toAnalyse.length - value) % value
          //println(s"first index = $firstIndexOfResultingList, from ${(toAnalyse.length - value - 1)} and ${toAnalyse.length - 1} : ${toAnalyse((toAnalyse.length - value - 1))} and ${toAnalyse.last}")
          //println(s"cycle is ${value} with drift of ${(toAnalyse.length-value)%value}");
          Some(Cycle(toAnalyse.takeRight(value), (firstIndexOfResultingList)))

class Cycle[A](values: List[A], moduloOfFirstElement: Int):
  val reArrangedList = moduloOfFirstElement match
    case 0 => values
    case value => values.takeRight(value) ::: values.dropRight(value)

  def getValueOf(index: Int): A =
    val calculatedIndex = index % values.length match
      case value if value >= moduloOfFirstElement => moduloOfFirstElement + value
      case value => (moduloOfFirstElement - value)

    //println(s"$calculatedIndex, $index, ${values.length}, $moduloOfFirstElement, $values, $reArrangedList ")
    //println(s"${index%values.length}, $index, ${values.length}, $moduloOfFirstElement, $values, $reArrangedList  ==> ${reArrangedList(index%values.length)} <==")
    reArrangedList(index%values.length)