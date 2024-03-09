import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val mainModules = inputLines.map:
      case s"%$moduleName -> $destinations" => FlipFlop(moduleName, destinations.toListOfModuleNames)
      case s"&$moduleName -> $destinations" => Conjunction(moduleName, destinations.toListOfModuleNames, Nil)
      case s"$moduleName -> $destinations" => Broadcaster(moduleName, destinations.toListOfModuleNames)
      case _ => throw Exception("Not managed")

    val outputModules = (mainModules.flatMap(_.getDestinations).distinct diff mainModules.map(_.getName)).map(Output(_, Nil))
    val allModules = mainModules ++: outputModules

    allModules.foreach:
      case module: WithInputs => module.addInputs(mainModules.toList.filter(_.getDestinations.contains(module.getName)).map(_.getName))
      case _ => ()

    val modulesMap = allModules.map(current => current.getName -> current).toMap
    given ModuleMap = modulesMap

    val warmupCycles = 1000

    val resultPart1 = push(warmupCycles)
    val resultPart2 =  allModules.size match
      case 6 => "not relevant"
      case _ => findFrequencies("rx", warmupCycles).product

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

val firstPulse = Pulse("broadcaster", "broadcaster", Low)

def push(nTimes: Int)(using ModuleMap): Long =
  @tailrec
  def propagate(pulses: List[Pulse], lows: Long, highs: Long, currentIter: Int): (Long, Long) =
    pulses match
      case Nil => (lows, highs)
      case head :: tail =>
        val newPulses:List[Pulse] = summon[ModuleMap](head.destination).manage(head)
        val (lowsList, highsList) = newPulses.partition(_.pulseType == Low)
        propagate(tail ::: newPulses, lows + lowsList.length, highs + highsList.length, currentIter)

  val (lows, highs) = (1 to nTimes).foldLeft((0l, 0l)):
    (acc, iter) => propagate(List(firstPulse), acc._1 + 1, acc._2, iter)

  lows * highs

def findFrequencies(outputName: String, startAt: Int)(using ModuleMap): List[Long] =
  import scala.collection.mutable.Map
  @tailrec
  def waitUntil(pulses: List[Pulse], currentIter: Int, inputModuleNames: List[ModuleName], inputsStatuses: Map[ModuleName, Int]): List[Int] =
    inputsStatuses.values.forall(_ != 0) match
      case true => inputsStatuses.values.toList
      case false =>
        pulses match
          case Nil => waitUntil(List(firstPulse), currentIter+1, inputModuleNames, inputsStatuses)
          case head :: tail =>
            val newPulses:List[Pulse] = summon[ModuleMap](head.destination).manage(head)
            inputModuleNames.foreach:
              summon[ModuleMap].get(_).foreach:
                case module: Conjunction =>
                  module.lastPulses.filter(_._2 == High).foreach:
                    (currentInput, _) =>
                      if (inputsStatuses.get(currentInput).fold(false)(_ == 0))
                        inputsStatuses.update(currentInput, currentIter)
                case _ => ()
            waitUntil(tail ::: newPulses, currentIter, inputModuleNames, inputsStatuses)

  val inputModuleNames = summon[ModuleMap](outputName) match
    case output: Output => output.inputs
    case _ => throw Exception("Output was not found")
  val inputs = inputModuleNames.flatMap:
    summon[ModuleMap](_) match
      case conjunction: Conjunction => conjunction.inputs
      case _ => throw Exception("Not possible")

  waitUntil(List(firstPulse), startAt + 1, inputModuleNames, Map(inputs.map(_ -> 0):_*)).map(_.toLong)

enum PulseType:
  case Low, High

export PulseType.*

type ModuleMap = Map[ModuleName, Module]

type ModuleName = String

case class Pulse(source: ModuleName, destination: ModuleName, pulseType: PulseType)

sealed trait Module:
  def getName: String
  def getDestinations: List[ModuleName]
  def manage(pulse: Pulse): List[Pulse]
  def generateToAll(pulseType: PulseType): List[Pulse] = getDestinations.map(Pulse(this.getName, _, pulseType))

sealed trait WithInputs:
  def addInputs(newInputs: List[ModuleName]): Unit

class Output(val name: String, var inputs: List[ModuleName]) extends Module with WithInputs:
  override def getDestinations: List[ModuleName] = Nil
  override def getName: String = name
  override def manage(pulse: Pulse): List[Pulse] = Nil

  override def addInputs(newInputs: List[ModuleName]): Unit =
    inputs = inputs ::: newInputs

class Broadcaster(val name: String, val destinations: List[ModuleName]) extends Module:
  override def getDestinations: List[ModuleName] = destinations
  override def getName: _root_.java.lang.String = name
  override def manage(pulse: Pulse): List[Pulse] = generateToAll(pulse.pulseType)

class FlipFlop(val name: String, val destinations: List[ModuleName]) extends Module:
  override def getDestinations: List[ModuleName] = destinations
  override def getName: String = name
  private var status: Boolean = false
  private def switch(): Unit = status = !status

  override def manage(pulse: Pulse): List[Pulse] =
    pulse.pulseType match
      case High => Nil
      case Low =>
        val pulseType = status match
          case true => Low
          case false => High
        switch()
        generateToAll(pulseType)

class Conjunction(val name: String, val destinations: List[ModuleName], var inputs: List[ModuleName]) extends Module with WithInputs:
  override def getDestinations: List[ModuleName] = destinations
  override def getName: String = name
  import scala.collection.mutable.Map
  lazy val lastPulses: Map[String, PulseType] = Map(inputs.map(_ -> Low):_*)
  private def updateLastPulses(pulse: Pulse): Unit = lastPulses.update(pulse.source, pulse.pulseType)

  override def addInputs(newInputs: List[ModuleName]): Unit =
    inputs = inputs ::: newInputs

  override def manage(pulse: Pulse): List[Pulse] =
    updateLastPulses(pulse)
    val pulseType = lastPulses.forall(_._2 == High) match
      case true => Low
      case false => High
    generateToAll(pulseType)

extension (destinations: String)
  def toListOfModuleNames: List[ModuleName] = destinations.split(", ").toList