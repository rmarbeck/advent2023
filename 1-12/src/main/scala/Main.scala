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
  println("Launching 1-12")
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

    val part1 = lines.map(_.filter(_.isDigit) match
      case value if !value.isEmpty => s"${value.head}${value.last}".toInt
      case _ => 0
    ).sum

    val part2 = lines.map { currentLine =>
      (findInProgressiveList(currentLine, true), findInProgressiveList(currentLine, false)) match
        case (Some(first), Some(second)) =>
          val foundResult = s"${first}${second}".toInt
          loggerAOCPart2.debug(s"  -  ${foundResult}")
          foundResult
        case _ => println("NOT POSSIBLE"); 0
    }.sum

    val (result1, result2) = (s"$part1", s"$part2")

    (s"${result1}", s"${result2}")

def findInProgressiveList(rawValue: String, leftToRight: Boolean): Option[Int] =
  progessiveList(rawValue, 0, leftToRight).find(_.isDefined).get

def progessiveList(rawValue: String, position: Int, leftToRight: Boolean): LazyList[Option[Int]] =
  def foundNumber(onPosition: Int) = leftToRight match
    case true => findOneNumber(rawValue.take(onPosition))
    case false => findOneNumber(rawValue.takeRight(onPosition))
  if (foundNumber(position).isDefined) LazyList.empty
  else LazyList.cons(foundNumber(position+1), progessiveList(rawValue, position+1, leftToRight))

def findOneNumber(toLookIn: String): Option[Int] =
  loggerAOCPart2.trace(s"$toLookIn")
  val asLetters = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex
  ('1' to '9').find(toLookIn.contains(_)) match
    case Some(valueAsInt) => Some(valueAsInt.asDigit)
    case None =>
      asLetters.find((value, index) => toLookIn.contains(value)) match
        case Some(valueAsLetters, index) => Some(index+1)
        case None => None
