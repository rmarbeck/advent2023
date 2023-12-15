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

sealed class Command(label: String):
  def key = hash(label)

case class Remove(label: String) extends Command(label)
case class AddOrUpdate(label: String, lensValue: Int) extends Command(label)

object Command:
  def fromString(input: String): Command =
    input match
      case s"${label}=${value}" => AddOrUpdate(label, value.toInt)
      case s"${label}-" => Remove(label)

class Container:
  def calc: Long =
    boxes.zipWithIndex.foldLeft(0l) { (acc, newValue) =>
      val (box, index) = (newValue._1, newValue._2 + 1)
      acc + (index * box.calc)
    }

  val boxes: Array[Box] = Array.fill(256)(Box(0, Seq()))
  (0 until boxes.length).foreach(index => boxes(index) = Box(index, boxes(index)))

  def manageCommand(command: Command) =
    command match
      case value@Remove(label) => boxes(value.key) = boxes(value.key).remove(label)
      case value@AddOrUpdate(label, lensValue) => boxes(value.key) = boxes(value.key).add(label, lensValue)
      case _ => ()

  override def toString(): String =
    boxes.filterNot(_.isEmpty).map(_.toString()).mkString("\n")

class Box(id: Int, val lenses: Seq[Lens]):
  def calc: Long =
    lenses.zipWithIndex.foldLeft(0l) { (acc, newValue) =>
      val (lens, index) = (newValue._1, newValue._2 + 1)
      acc + (index * lens.lensValue)
    }

  def this(id: Int, box: Box) = this(id, box.lenses)

  def isEmpty: Boolean = lenses.isEmpty
  def remove(label: String): Box =
    Box(id, this.lenses.filterNot(_.label == label))

  def add(label: String, newLensValue: Int): Box =
    val toAdd = this.lenses.find(currentLens => currentLens.label == label).isEmpty
    Box(id, toAdd match
      case true => this.lenses :+ Lens(label, newLensValue)
      case false => this.lenses.map { currentLens =>
        currentLens match
          case value if value.label == label => Lens(label, newLensValue)
          case _ => currentLens
      })

  override def toString(): String =
    s"Box $id: ${lenses.mkString(" ")}"

case class Lens(label: String, lensValue: Int):
  override def toString(): String =
    s"[${label} ${lensValue}]"

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq

    val result = lines(0).split(",").map(hash).sum

    val container = Container()

    lines(0).split(",").foreach(current => container.manageCommand(Command.fromString(current)))
    //println(container)

    val (result1, result2) = (s"$result", s"${container.calc}")

    (s"${result1}", s"${result2}")

def hash(input: String): Int =
  input.foldLeft(0) { (acc, current) =>
    ((acc + current.toInt) * 17 ) % 256
  }
