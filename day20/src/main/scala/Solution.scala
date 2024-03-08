object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val modules = inputLines.map:
      case s"%$moduleName -> $destinations" => FlipFlop(moduleName, destinations.split(", ").toList)
      case s"&$moduleName -> $destinations" => Conjunction(moduleName, destinations.split(", ").toList, Nil)
      case s"$moduleName -> $destinations" => Broadcaster(moduleName, destinations.split(", ").toList)
      case _ => throw Exception("Not managed")

    modules.foreach:
      case conjunction: Conjunction => conjunction.addInputs(modules.toList.filter(_.destinations.contains(conjunction)).map(_.name))
      case _ => ()

    val modulesMap = (Output("rx") +: modules).map(current => current.name -> current).toMap
    given ModuleMap = modulesMap

    val result1 = s"${push(1000)}"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

def push(nTimes: Int)(using ModuleMap): Long =
  def propagate(pulses: List[Pulse], lows: Long, highs: Long): (Long, Long) =
    pulses match
      case Nil => (lows, highs)
      case head :: tail =>
        val newPulses:List[Pulse] = summon[ModuleMap](head.destination).manage(head)
        val (lowsList, highsList) = newPulses.partition(_.pulseType == Low)
        propagate(tail ::: newPulses, lows + lowsList.length, highs + highsList.length)

  val (lows, highs) = (1 to nTimes).foldLeft((0l, 0l)):
    (acc, _) => propagate(List(Pulse("broadcaster", "broadcaster", Low)), acc._1 + 1, acc._2)

  lows * highs

enum PulseType:
  case Low, High

export PulseType.*

type ModuleMap = Map[ModuleName, Module]

type ModuleName = String

case class Pulse(source: ModuleName, destination: ModuleName, pulseType: PulseType)

sealed trait Module(val name: String, val destinations: List[ModuleName]):
  def manage(pulse: Pulse): List[Pulse]
  def generateToAll(pulseType: PulseType): List[Pulse] = destinations.map(Pulse(this.name, _, pulseType))


class Output(name: String) extends Module(name, Nil):
  override def manage(pulse: Pulse): List[Pulse] = Nil

class Broadcaster(name: String, destinations: List[ModuleName]) extends Module(name, destinations):
  override def manage(pulse: Pulse): List[Pulse] = generateToAll(pulse.pulseType)

class FlipFlop(name: String, destinations: List[ModuleName]) extends Module(name, destinations):
  private var status: Boolean = false
  private def switch(): Unit = status = !status

  override def manage(pulse: Pulse): List[Pulse] =
    pulse.pulseType match
      case High => Nil
      case Low =>
        switch()
        val pulseType = status match
          case true => Low
          case false => High
        generateToAll(pulseType)

class Conjunction(name: String, destinations: List[ModuleName], var inputs: List[ModuleName]) extends Module(name, destinations):
  import scala.collection.mutable.Map
  private lazy val lastPulses: Map[String, PulseType] = Map(inputs.map(_ -> Low):_*)
  private def updateLastPulses(pulse: Pulse): Unit = lastPulses.update(pulse.source, pulse.pulseType)

  def addInputs(newInputs: List[ModuleName]): Unit = inputs = inputs ::: newInputs

  override def manage(pulse: Pulse): List[Pulse] =
    updateLastPulses(pulse)
    val pulseType = lastPulses.forall(_._2 == High) match
      case true => Low
      case false => High
    generateToAll(pulseType)