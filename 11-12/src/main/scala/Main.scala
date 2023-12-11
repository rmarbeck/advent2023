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
    val space = bufferedSource.getLines().toSeq

    val listOfEmptyLines = findEmptyLines(space)
    val listOfEmptyColumns = findEmptyLines(space.transpose.map(_.mkString))

    val galaxies = (
      for j <- 0 until space(0).length
          i <- 0 until space.length
      yield
        space(i)(j) match
          case '#' => Some(i, j)
          case _ => None
        ).filter(_.isDefined).map(_.get)

    val solver = (expansionRate: Long) => galaxies.zipWithIndex.map { (galaxy1, index) =>
      val subResult = galaxies.drop(index+1).foldLeft(0l) { (acc, newValue: (Int, Int)) =>
        acc + distanceBetweenGalaxies(galaxy1, newValue, listOfEmptyLines, listOfEmptyColumns, expansionRate)
      }
      subResult
    }.sum

    val (result1, result2) = (s"${solver(2)}", s"${solver(1000000)}")

    (s"${result1}", s"${result2}")

def duplicateEmptyLines(input: Seq[String]): Seq[String] =
  input.flatMap { currentLine =>
    currentLine.filterNot(_.equals('.')) match {
      case value if value.isEmpty => Seq(currentLine, currentLine)
      case value => Seq(currentLine)
    }
  }

def findEmptyLines(input: Seq[String]): Seq[Int] =
  input.zipWithIndex.filter { (currentLine, index) =>
    currentLine.filterNot(_.equals('.')) match {
      case value if value.isEmpty => true
      case value => false
    }
  }.map((currentLine, index) => index)

def distanceBetweenGalaxies(galaxy1: (Int, Int), galaxy2: (Int, Int), listOfEmptyLines: Seq[Int], listOfEmptyColumns: Seq[Int], expansionRate: Long = 1000000): Long =
  val (coordXGalaxy1, coordYGalaxy1, coordXGalaxy2, coordYGalaxy2) = (galaxy1._1, galaxy1._2, galaxy2._1, galaxy2._2)
  val distanceBeforeExpansion = math.abs(coordXGalaxy2 - coordXGalaxy1) + math.abs(coordYGalaxy2 - coordYGalaxy1)
  val nbOfExpandedLinesBetweenGalaxies = listOfEmptyLines.filter { lineNumber =>
    lineNumber match
      case value if value < coordXGalaxy1 && value < coordXGalaxy2 => false
      case value if value > coordXGalaxy1 && value > coordXGalaxy2 => false
      case _ => true
  }.length
  val nbOfExpandedColumnsBetweenGalaxies = listOfEmptyColumns.filter { columnNumber =>
    columnNumber match
      case value if value < coordYGalaxy1 && value < coordYGalaxy2 => false
      case value if value > coordYGalaxy1 && value > coordYGalaxy2 => false
      case _ => true
  }.length

  val multiplier = expansionRate - 1

  val newDistance = distanceBeforeExpansion+(multiplier*nbOfExpandedLinesBetweenGalaxies)+(multiplier*nbOfExpandedColumnsBetweenGalaxies)
  newDistance