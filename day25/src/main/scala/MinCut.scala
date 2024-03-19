import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.SetIsParallelizable
import collection.parallel.CollectionConverters.seqIsParallelizable
import collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.util.Random

val rand = new Random

case class Summit(name: String)

class SimpleGraph(wireBox: WireBox) extends Graph[Component]:
  override lazy val getElements: Seq[Component] = wireBox.wires.flatMap(_.ends).distinct
  override lazy val nbOfEdges: Long = wireBox.wires.size * 2
  import collection.immutable.HashMap
  lazy val asInt: Map[Component, Int] = collection.immutable.HashMap(getElements.zipWithIndex.map((element, index) => element -> index):_*)

  override def merge(one: Component, other: Component): Graph[Component] =
    val mergedComponent = Component(s"$one-$other")
    val updatedWires =
      wireBox.wires.withFilter:
        wire => !(wire.ends.contains(one) && wire.ends.contains(other))
      .map:
        case wire if wire.ends.contains(one) =>
          val otherEnd = wire.ends.filterNot(_ == one).head
          Wire(Set(mergedComponent, otherEnd))
        case wire if wire.ends.contains(other) =>
          val otherEnd = wire.ends.filterNot(_ == other).head
          Wire(Set(mergedComponent, otherEnd))
        case wire => wire

    SimpleGraph(WireBox(updatedWires))

  import collection.mutable.{HashMap, Map}
  val connectedToMap: Map[Component, Seq[Int]] = collection.mutable.HashMap[Component, Seq[Int]]()
  def connectedTo(element: Component): Seq[Int] =
    connectedToMap.getOrElseUpdate(element, wireBox.wires.flatMap(_.otherThan(element)).map(asInt))

  override def getNeighboursOfIn(current: Int, potentialNeighbours: BitSet): Int =
    connectedTo(getElements(current)).foldLeft(0):
      (acc, newElement) => potentialNeighbours(newElement) match
        case true => acc + 1
        case false => acc

  override def findMostConnected(elementsOfSubGraph: BitSet, elementsToConnectTo: BitSet): Int =
    elementsOfSubGraph.toParArray.maxBy:
      element =>  getNeighboursOfIn(element, elementsToConnectTo)


trait Graph[T]:
  def getElements: Seq[T]
  def asInt: Map[T, Int]
  def merge(one: T, other: T): Graph[T]
  def nbOfEdges: Long
  def getNeighboursOfIn(current: Int, potentialNeighbours: BitSet): Int
  def findMostConnected(elementsOfSubGraph: BitSet, elementsToConnectTo: BitSet): Int


def MinCupStep[T](graph: Graph[T], startingElement: T): (Graph[T], Long) =
  def cutBetween(elementsOfSubGraph: BitSet, element: Int): Long = graph.getNeighboursOfIn(element, elementsOfSubGraph)

  def findMostConnected(elementsOfSubGraph: BitSet, elementsToConnectTo: BitSet): Int =
    graph.findMostConnected(elementsOfSubGraph, elementsToConnectTo)

  @tailrec
  def appendMostConnected(remainingElements: BitSet, appendingList: BitSet, headOfAppending: Int): (T, T, Long) =
    remainingElements.size match
      case 0 => throw Exception("Not supported")
      case 1 =>
        val cut = cutBetween(appendingList, remainingElements.head)
        (graph.getElements(headOfAppending), graph.getElements(remainingElements.head), cut)
      case _ =>
        val toAdd = findMostConnected(remainingElements, appendingList)
        appendMostConnected(remainingElements.excl(toAdd), appendingList.incl(toAdd), toAdd)

  val startingAppending = graph.asInt(startingElement)
  val rawRemaining = graph.getElements.map(graph.asInt).filterNot(_ == startingAppending)
  val startingRemaining = BitSet(rawRemaining:_*)

  val (t, s, cut ) = appendMostConnected(startingRemaining, BitSet(startingAppending), startingAppending)
  (graph.merge(t, s), cut)

def MinCut[T](graph: Graph[T], startingElement: T): (Long, Long) =
  def callStep(graph: Graph[T], startingElement: T, bestCutGraphSize: Long, cutValue: Long): (Long, Long) =
    graph.getElements.size match
      case size if size < 1 => (bestCutGraphSize, cutValue)
      case _ =>
        val (resultingGraph, cut) = MinCupStep(graph, startingElement)
        println(s"Stepping $cutValue $bestCutGraphSize vs $cut and ${resultingGraph.getElements.size}")
        if (cut < cutValue)
          callStep(resultingGraph, startingElement, resultingGraph.getElements.size, cut)
        else
          callStep(resultingGraph, startingElement, bestCutGraphSize, cutValue)


  val sizeOfInitialGraph = graph.getElements.size

  callStep(graph, startingElement, sizeOfInitialGraph, graph.nbOfEdges)
