import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.math.*
import scala.collection.mutable.Map
import java.time.Duration
import java.time.Instant
import scala.annotation.tailrec
// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

import Pulse._

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching 20-12")
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

    val container = Container(lines)

    /*val (lows, higths) = (
      for
        index <- 1 to 1000
      yield
        container.resetGeneric
        val result = container.start
        container.displayGeneric
        result
    ).unzip*/

    //val result = lows.sum * higths.sum

    val (nbLows, nbHights, nbOfCycles) = lazyList(container, List()).take(1000).foldLeft((0, 0, 0)) {(acc, newValue) =>
      (acc._1+newValue._1, acc._2+newValue._2, acc._3 + 1)
    }

    println(s"${(nbLows, nbHights, nbOfCycles)}")

    val result = ((nbLows*1000/nbOfCycles)*(nbHights*1000/nbOfCycles))

    val container2 = Container(lines)

    val resultb = waitLow(container2, 0)
    //val resultb =" "

    val (result1, result2) = (s"$result", s"$resultb")



    (s"${result1}", s"${result2}")

class Container(input: Seq[String]):
  val modules: Map[String, Module] = Map[String, Module]()
  val (moduleNames: Seq[String], destinations: Seq[List[String]]) = input.map:
    case s"$moduleName -> $destinations" => (moduleName, destinations.split(", ").toList)
  .unzip
  moduleNames.foreach:
    case s"%$fliFlopName" => modules.put(fliFlopName, FlipFlop(fliFlopName))
    case s"&$conjunctionName" => modules.put(conjunctionName, Conjunction(conjunctionName))
    case s"broadcaster" => modules.put("broadcaster", Broadcaster())
    case s"$name" => modules.put("name", Generic(name))


  destinations.flatten.distinct.map(currentName => modules.getOrElseUpdate(currentName, Generic(currentName)))

  destinations.zip(moduleNames.map(_.replaceAll("[\\%&]", ""))).foreach:
    case (destinationNames, moduleName) => modules.get(moduleName).foreach {currentSourceModule =>
      destinationNames.map(modules.get(_)).filter(_.isDefined).foreach {
        case Some(moduleToAppend) =>
          currentSourceModule.destinations.append(moduleToAppend)
      }
    }

  modules.foreach:
    case (conjunctionName, conjunctionModule: Conjunction) =>
      modules.filter((key, module) => module.destinations.modules.map(_.name).contains(conjunctionName)).map(_._2).foreach:
        case module: Module => conjunctionModule.addInput(module)
    case _ => ()

  def getGeneric: Int =
    modules.get("rx").map:
      case value: Generic => value.count
      case _ => -1
    .getOrElse(-1)

  def isGenericASuccess: Boolean =
    modules.get("rx").map:
      case value: Generic if (value.count == 1) => true
      case _ => false
    .getOrElse(true)

  def displayGeneric =
    modules.get("rx").map:
      case value: Generic =>
        if (value.count == 1)
          println("WINNNNNNNNNNNN")
        println(value.count)
      case _ => println(-1)

  def resetGeneric =
    modules.get("rx").map:
      case value: Generic => value.reset
      case _ => println("Never happen")

  def status: String =
    modules.values.map(_.status).flatten.map:
      case true => "1"
      case false => "0"
    .mkString

  def start: (Int, Int) =
    send(List(Flow(Low, modules.get("broadcaster").get, modules.get("broadcaster").get)), 1, 0)

  def send(inputs: List[Flow], lows: Int, highs: Int): (Int, Int) =
    inputs match
      case Nil => (lows, highs)
      case head :: tail =>
        val newFlows = send(head)
        val (newLows, newHighs) = (newFlows.filter(_.pulse == Low).length, newFlows.filter(_.pulse == High).length)
        send(tail ::: newFlows, lows + newLows, highs + newHighs)

  def send(flow: Flow): List[Flow] =
    loggerAOC.trace(s"sending ${flow.pulse} from ${flow.moduleSource}  to ${flow.moduleDestination}")
    modules.get(flow.moduleDestination.name) match
      case Some(module: Module) =>
        module.reactTo(flow)
      case _ =>
        List()

  override def toString: String = s"Container : ${modules.toList.map((key, value) => s"\n [$key] => \t\t${value.toString}")}"

class Destinations(private var destinations: List[Module]):
  def append(module: Module) = destinations = destinations :+ module
  def modules: List[Module] = destinations
  override def toString: String = s"[${destinations.map(_.name).mkString("<->")}]"

object Destinations:
  def default = Destinations(List())

case class Flow(pulse: Pulse, moduleSource: Module, moduleDestination: Module)

class Switch(var status: Boolean):
  override def toString: String = s"${if status then "on" else "off" }"
  def toggle =
    //loggerAOC.trace(s"toggling from $this")
    status = !status

sealed abstract class Module(val name: String, val destinations: Destinations = Destinations.default):
  def status: List[Boolean] = List()
  def reactTo(flow: Flow) : List[Flow] =
    val result = doReactTo(flow)
    //println(s"sending ${result.filter(_.pulse == Low).length} Low, and ${result.filter(_.pulse == High).length} High")
    result

  def doReactTo(flow: Flow) : List[Flow]

case class FlipFlop(override val name: String, switch: Switch = Switch(false)) extends Module(name):
  override def status: List[Boolean] = List(switch.status)
  override def doReactTo(flow: Flow): List[Flow] =
    flow.pulse match
      case Low if switch.status == true =>
        loggerAOC.trace(s"$name toggling from ${switch}")
        switch.toggle
        destinations.modules.map(Flow(Low, this, _))
      case Low =>
        loggerAOC.trace(s"$name toggling from ${switch}")
        switch.toggle
        destinations.modules.map(Flow(High, this, _))
      case _ => Nil

  override def toString: String = s"FlipFlop[$name](${switch} ${destinations})"

case class Conjunction(override val name: String, lastInputs: Map[String, Pulse] = Map[String, Pulse]()) extends Module(name):
  override def status: List[Boolean] = lastInputs.values.map(currentPulse => if currentPulse  == Low then false else true).toList
  def addInput(moduleSource: Module) =
    lastInputs.put(moduleSource.name, Low)
  def updateSwitchFromModule(module: Module, pulse: Pulse): Unit =
    lastInputs.get(module.name) match
      case Some(value) => lastInputs.put(module.name, pulse)
      case None => loggerAOC.debug(s"Should never happen : ${module}"); lastInputs.put(module.name, pulse)

  override def doReactTo(flow: Flow): List[Flow] =
    loggerAOC.trace(s"In ${this} ${this.lastInputs.head}, flow is from ${flow.moduleSource}")
    if (name == "jq")
      if (lastInputs.filter((_, pulse) => pulse == High).toList.length > 0)
        loggerAOC.trace(s"${lastInputs.values.map(value => if value == Low then "0" else "1").mkString}")

    updateSwitchFromModule(flow.moduleSource, flow.pulse)
    lastInputs.filterNot((_, pulse) => pulse == High).toList.length == 0 match
      case true =>
        destinations.modules.map(Flow(Low, this, _))
      case false =>
        destinations.modules.map(Flow(High, this, _))

  override def toString: String = s"Conjunction[$name](#${lastInputs.keys.mkString(",")}##${lastInputs.values.map(value => if value == Low then "L" else "H").mkString(",")}# ${destinations})"

case class Broadcaster() extends Module("broadcaster"):
  override def doReactTo(flow: Flow): List[Flow] =
    val result = destinations.modules.map(Flow(flow.pulse, this, _))
    result

  override def toString: String = s"Broadcaster(${destinations})"


case class Generic(override val name: String) extends Module(name):
  override def status = List(count == 1)
  def reset = count = 0
  var count = 0
  override def doReactTo(flow: Flow): List[Flow] =
    if (flow.pulse == Low)
      println("low")
      count = count+1
    Nil

  override def toString: String = s"Generic()"

def lazyList(container: Container, statuses: List[String]): LazyList[(Int, Int)] =
  val result = container.start
  val currentStatus = container.status

  if statuses.find(_ == currentStatus).isDefined then LazyList.empty
  else LazyList.cons(result, lazyList(container, currentStatus +: statuses))


var results = Map[String, Int]()
@tailrec
def waitLow(container: Container, steps: Int): Int =
  if (steps == 0)
    results = Map[String, Int]()
    container.resetGeneric
  container.start
  val status = container.status
  results.put(status, results.getOrElseUpdate(status, 0) + 1)
  if container.isGenericASuccess  || steps == 900000 then results.values.toList.length
  else waitLow(container, steps + 1)

def lazyList2(container: Container, steps: Int): LazyList[Int] =
  if (steps == 0)
    container.resetGeneric
  container.start

  if container.isGenericASuccess then LazyList.empty
  else LazyList.cons(1, lazyList2(container, steps+1))

enum Pulse:
  case Low, High