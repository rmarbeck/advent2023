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
  println("Launching 25-12")
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



    val wires = lines.flatMap:
      case s"$end1: $ends" =>
        ends.split(" ").map(currentEnd => Wire(Component(currentEnd), Component(end1)))

    val box = WiresBox(wires)
    val graph = Graph(box.vertices, box.edges)
    val result = MinCutCalculator(false).calculateMinCut(graph)

    val (result1, result2) = (s"$result", "")

    (s"${result1}", s"${result2}")

case class Component(name: String):
  def toInt: Int =
    name.map(_.toInt).foldLeft(0) {(acc, value) => acc * 100 + value}

case class Wire(component1: Component, component2: Component):
  def contains(component: Component): Boolean =
    component match
      case value if value == component1 || value == component2 => true
      case _ => false
  def canConnectTo(chain: Chain): Boolean =
    chain.length == 0 match
      case true => true
      case false => chain.contains(component1) || chain.contains(component2)

  override def equals(obj: Any): Boolean =
    obj match
      case Wire(value1, value2) if value1 == component1 && value2 == component2 => true
      case Wire(value1, value2) if value1 == component2 && value2 == component1 => true
      case _ => false

object Wire:
  def of(component1: String, component2: String) =
    Wire(Component(component1), Component(component2))

class Chain:
  var components: Set[Component] = Set[Component]()
  def addWire(wire: Wire): Chain =
    components += wire.component1
    components += wire.component2
    this
  def length: Int = components.size
  def contains(component: Component): Boolean = components.contains(component)


class WiresBox(wiresInput: Seq[Wire]):
  var wires: Set[Wire] = wiresInput.toSet
  def vertices: Vector[Set[Int]] = wires.flatMap(current => Set(current.component1, current.component2)).map(_.toInt).toVector.map(current => Set(current))
  def edges: Vector[(Int, Int)] = wires.map(current => (current.component1.toInt, current.component2.toInt)).toVector
  def openEnds: Int =
    wires.flatMap(current => Set(current.component1, current.component2)).filter: component =>
      wires.filter(_.contains(component)).size == 1
    .size

@tailrec
def inTwoGroups(wires: Seq[Wire], currentChain: Chain, groupsSizes: List[Int]): Option[(Int, Int)] =
  def manageEnd(updatedChain: Chain, updatedGroupsSizes: List[Int]): Option[(Int, Int)] =
    groupsSizes.length match
      case length if length == 1 && updatedChain.length != 0 => Some((updatedGroupsSizes.head, updatedChain.length))
      case length if length == 2 && updatedChain.length == 0 => Some((updatedGroupsSizes.head, updatedGroupsSizes.drop(1).head))
      case _ => None
  groupsSizes.length > 2 match
    case true => None
    case _ =>
      wires match
        case Nil => manageEnd(currentChain, groupsSizes)
        case head :: Nil =>
          head.canConnectTo(currentChain) match
            case true => manageEnd(currentChain.addWire(head), groupsSizes)
            case false => manageEnd(Chain().addWire(head), groupsSizes :+ currentChain.length)
        case value =>
          val (newWires, newChain, newGroupSizes) = value.find(_.canConnectTo(currentChain)) match
            case Some(foundWire) => (wires.filterNot(_ == foundWire), currentChain.addWire(foundWire), groupsSizes)
            case None => (wires, Chain(), groupsSizes :+ currentChain.length)
          inTwoGroups(newWires, newChain, newGroupSizes)