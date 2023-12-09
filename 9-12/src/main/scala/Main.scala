import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
import scala.annotation.tailrec
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
    val inputs: Seq[Seq[Int]] = lines.map(_.split(" ").toSeq.map(_.toInt))

    val (part1, part2) = (inputs, inputs.map(_.reverse))

    Seq(part1, part2).map {
      _.map(computeWithLazyList(_)).sum
    }.foreach(result => println(s"lazy => $result"))

    val (result1, result2) = Seq(part1, part2).map {
      _.map(compute(_, 0)).sum }
    match { case Seq(a, b) => (a, b) }

    (s"${result1}", s"${result2}")

@tailrec
def compute(inputList: Seq[Int], currentSum: Long): Long =
  loggerAOC.trace(s"$currentSum")
  inputList.filterNot(_==0).length match
    case 0 => currentSum
    case _ => compute(inputList.sliding(2, 1).map((values) => values(1)-values(0)).toSeq, currentSum + inputList.takeRight(1).head.toLong)

def computeWithLazyList(inputList: Seq[Int]): Long =
  def diffOfElements(input: Seq[Int]): LazyList[Seq[Int]] = {
    def computed = input.sliding(2, 1).map((values) => values(1)-values(0)).toSeq
    if (input.filterNot( _ == 0 ).length == 0) LazyList.empty
    else LazyList.cons(input, diffOfElements(computed))
  }
  diffOfElements(inputList).map(_.last).sum
