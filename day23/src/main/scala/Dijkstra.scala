import scala.annotation.tailrec

class GraphFromListWithNexts(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit])(using summitsHolder: SummitsHolder) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = summitsHolder.distanceBetween(first.getElement, second.getElement)
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Seq[Data[Summit]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    potentialNeighbours.filter: summitWithData =>
      possibleNeighbours.contains(summitWithData.getElement)

trait Graph[T]:
  def getElements: Seq[T]
  def getListToExploreInitialisedFromStart(startingFrom: T): Seq[Data[T]] =
    val data = getElements.filterNot(_ == startingFrom).map(Data[T](_))
    Data(startingFrom, 0) +: data
  def weightBetween(first: Data[T], second: Data[T]): Long
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: Seq[Data[T]]): Seq[Data[T]]

class Data[T](element: T, private var currentDistance: Long = Long.MaxValue):
  def getElement = element
  private var precedingElement: Option[Data[T]] = None
  def getPreceding: Data[T] = precedingElement.get
  def getCurrentDistance: Long = currentDistance
  def updateDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    currentDistance = newDistance
    precedingElement = Some(preceding)

  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solve[T](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): Int =
    doSolve[T](graph.getListToExploreInitialisedFromStart(startingFrom).toList, Nil, elementsToReach)(graph)

  @tailrec
  private def doSolve[T](toExplore: List[Data[T]], explored: List[Data[T]], elementsToReach: List[T])(implicit graph: Graph[T]): Int =
    toExplore match
      case Nil => rewind(explored.head)
      case _ if  ! (explored.map(_.getElement) intersect elementsToReach).isEmpty => rewind(explored.head)
      case best :: tail =>
        graph.getNeighboursOfIn(best, toExplore).foreach: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance > distance)
            neighbour.updateDistanceAndPreceding(distance, best)

        doSolve(tail.sortBy(_.getCurrentDistance), best :: explored, elementsToReach)

  @tailrec
  private def rewind[T](current: Data[T], counter: Int=0): Int =
    current.getCurrentDistance match
      case 0 => counter
      case _ => println(current); rewind(current.getPreceding, math.min(counter, current.getCurrentDistance.toInt))