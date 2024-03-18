
case class Summit(name: String)

class SimpleGraph(wireBox: WireBox) extends Graph[Component]:
  override lazy val getElements: Seq[Component] = wireBox.wires.flatMap(_.ends).distinct
  override lazy val nbOfEdges: Long = wireBox.wires.size * 2

  override def merge(one: Component, other: Component): Graph[Component] =
    val mergedComponent = Component(s"$one-$other")
    val updatedWires = wireBox.wires.filterNot(_ == Set(one, other)).map:
      case wire if wire.ends.contains(one) =>
        val otherEnd = wire.ends.filterNot(_ == one).head
        Wire(Set(mergedComponent, otherEnd))
      case wire if wire.ends.contains(other) =>
        val otherEnd = wire.ends.filterNot(_ == other).head
        Wire(Set(mergedComponent, otherEnd))
      case wire => wire

    SimpleGraph(WireBox(updatedWires))

  override def getNeighboursOfIn(current: Component, potentialNeighbours: Seq[Component]): Seq[Component] =
    wireBox.wires.withFilter(_.ends.contains(current)).withFilter(currentWire => (currentWire.ends intersect potentialNeighbours.toSet).size != 0).map:
      wire => wire.ends.filter(_ != current).head


trait Graph[T]:
  def getElements: Seq[T]
  def merge(one: T, other: T): Graph[T]
  def nbOfEdges: Long
  def getNeighboursOfIn(current: T, potentialNeighbours: Seq[T]): Seq[T]


def MinCupStep[T](graph: Graph[T], startingElement: T): (Graph[T], Long) =
  val a: List[T] = List(startingElement)

  def cutBetween(elementsOfSubGraph: List[T], element: T, fullGraph: Graph[T]): Long =
    val elementsToConnectTo = fullGraph.getElements diff elementsOfSubGraph
    graph.getNeighboursOfIn(element, elementsToConnectTo).size

  def findMostConnected(elementsOfSubGraph: List[T], fullGraph: Graph[T]): T =
    val elementsToConnectTo = fullGraph.getElements diff elementsOfSubGraph
    elementsOfSubGraph.maxBy:
      element => graph.getNeighboursOfIn(element, elementsToConnectTo).size


  def appendMostConnected(remainingElements: List[T], appendingList: List[T], fullGraph: Graph[T]): (T, T, Long) =
    remainingElements match
      case Nil => throw Exception("Not supported")
      case head :: Nil =>
        val cut = cutBetween(appendingList, head, fullGraph)
        (appendingList.head, head, cut)
      case head :: tail =>
        val toAdd = findMostConnected(remainingElements, fullGraph)
        appendMostConnected(remainingElements.filterNot(_ == toAdd), toAdd +: appendingList, fullGraph)

  val (t, s, cut ) = appendMostConnected(graph.getElements.toList.filterNot(_ == startingElement), List(startingElement), graph)
  (graph.merge(t, s), cut)

def MinCut[T](graph: Graph[T], startingElement: T): (Long, Long) =
  def callStep(graph: Graph[T], startingElement: T, bestCutGraphSize: Long, cutValue: Long): (Long, Long) =
    graph.getElements.size match
      case 0 => (bestCutGraphSize, cutValue)
      case _ =>
        val (resultingGraph, cut) = MinCupStep(graph, startingElement)
        if (cut < cutValue)
          callStep(resultingGraph, startingElement, resultingGraph.getElements.size, cut)
        else
          callStep(resultingGraph, startingElement, bestCutGraphSize, cutValue)


  val sizeOfInitialGraph = graph.getElements.size

  callStep(graph, startingElement, sizeOfInitialGraph, graph.nbOfEdges)
