import scala.collection.immutable.BitSet

case class Summit(name: String)

class SimpleGraph(wireBox: WireBox) extends Graph[Component]:
  override lazy val getElements: Seq[Component] = wireBox.wires.flatMap(_.ends).distinct
  override lazy val nbOfEdges: Long = wireBox.wires.size * 2
  lazy val asInt: Map[Component, Int] = getElements.zipWithIndex.map((element, index) => element -> index).toMap

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

  override def getNeighboursOfIn(current: Component, potentialNeighbours: Seq[Component]): Seq[Component] =
    val potentials = BitSet(potentialNeighbours.map(asInt):_*)
    getNeighboursOfIn(current, potentials)

  def getNeighboursOfIn(current: Component, potentialNeighbours: BitSet): Seq[Component] =
    wireBox.wires.flatMap(_.otherThan(current)).map(asInt).filter(potentialNeighbours.apply).map(getElements)

  override def findMostConnected(elementsOfSubGraph: List[Component], elementsToConnectTo: List[Component]): Component =
    val elementsToConnectToAsInt = BitSet(elementsToConnectTo.map(asInt):_*)
    elementsOfSubGraph.maxBy:
      element =>  getNeighboursOfIn(element, elementsToConnectToAsInt).size


trait Graph[T]:
  def getElements: Seq[T]
  def merge(one: T, other: T): Graph[T]
  def nbOfEdges: Long
  def getNeighboursOfIn(current: T, potentialNeighbours: Seq[T]): Seq[T]
  def findMostConnected(elementsOfSubGraph: List[T], elementsToConnectTo: List[T]): T


def MinCupStep[T](graph: Graph[T], startingElement: T): (Graph[T], Long) =
  def cutBetween(elementsOfSubGraph: List[T], element: T): Long = graph.getNeighboursOfIn(element, elementsOfSubGraph).size

  def findMostConnected(elementsOfSubGraph: List[T], elementsToConnectTo: List[T]): T =
    graph.findMostConnected(elementsOfSubGraph, elementsToConnectTo)


  def appendMostConnected(remainingElements: List[T], appendingList: List[T], fullGraph: Graph[T]): (T, T, Long) =
    remainingElements match
      case Nil => throw Exception("Not supported")
      case head :: Nil =>
        val cut = cutBetween(appendingList, head)
        (appendingList.head, head, cut)
      case head :: tail =>
        val toAdd = findMostConnected(remainingElements, appendingList)
        println(s"$toAdd (${remainingElements.size})")
        appendMostConnected(remainingElements.filterNot(_ == toAdd), toAdd +: appendingList, fullGraph)

  val (t, s, cut ) = appendMostConnected(graph.getElements.toList.filterNot(_ == startingElement), List(startingElement), graph)
  (graph.merge(t, s), cut)

def MinCut[T](graph: Graph[T], startingElement: T): (Long, Long) =
  def callStep(graph: Graph[T], startingElement: T, bestCutGraphSize: Long, cutValue: Long): (Long, Long) =
    graph.getElements.size match
      case size if size < 1 => (bestCutGraphSize, cutValue)
      case _ =>
        val (resultingGraph, cut) = MinCupStep(graph, startingElement)
        if (cut < cutValue)
          callStep(resultingGraph, startingElement, resultingGraph.getElements.size, cut)
        else
          callStep(resultingGraph, startingElement, bestCutGraphSize, cutValue)


  val sizeOfInitialGraph = graph.getElements.size

  callStep(graph, startingElement, sizeOfInitialGraph, graph.nbOfEdges)
