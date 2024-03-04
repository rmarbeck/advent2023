import scala.annotation.tailrec

enum Direction:
  def opposite: Direction =
    this match
      case Left2Right => Right2Left
      case Right2Left => Left2Right
      case Up2Down => Down2Up
      case Down2Up => Up2Down
  case Left2Right, Right2Left, Up2Down, Down2Up



export Direction.*

trait Valued:
  def getValue: Int

case class Summit(row: Int, col: Int, value: Int, lastDir: Direction, lastDirCounter: Int) extends Valued:
  override def getValue: Int = lastDirCounter + 1

class GraphFromArray(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = second.getElement.value
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Map[Summit, Data[Summit]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    possibleNeighbours.flatMap(potentialNeighbours.get)

trait Graph[T <: Valued]:
  def getElements: Seq[T]
  def getListToExploreInitialisedFromStart(startingFrom: T): Seq[Data[T]] =
    val data = getElements.filterNot(_ == startingFrom).map(Data[T](_))
    Data(startingFrom, 0) +: data
  def weightBetween(first: Data[T], second: Data[T]): Long
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: Map[T, Data[T]]): Seq[Data[T]]

class Data[T <: Valued](element: T, private var currentDistance: Long = Long.MaxValue):
  def getElement = element
  private var precedingElement: Option[Data[T]] = None
  def getPreceding: Data[T] = precedingElement.get
  def getCurrentDistance: Long = currentDistance
  def getBestFirst: (Long, Int) = (currentDistance, element.getValue)
  def updateDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    currentDistance = newDistance
    precedingElement = Some(preceding)

  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solve[T <: Valued](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): List[T] =
    val toExplore = graph.getListToExploreInitialisedFromStart(startingFrom).toList
    val mapOfSummits = toExplore.map(summitData => summitData.getElement -> summitData).toMap
    doSolve[T](toExplore.toVector, Nil, elementsToReach, mapOfSummits)(graph)

  @tailrec
  private def doSolve[T <: Valued](toExplore: Vector[Data[T]], explored: List[Data[T]], elementsToReach: List[T], neighboursMap : Map[T, Data[T]])(implicit graph: Graph[T]): List[T] =
    toExplore match
      case value if value.isEmpty => rewind(explored.head)
      case _ if  (explored.map(_.getElement) intersect elementsToReach).nonEmpty => println(explored.length); rewind(explored.head)
      case value =>
        val best = toExplore.head
        val tail = toExplore.tail
        println(best)
        graph.getNeighboursOfIn(best, neighboursMap).foreach: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance > distance)
            neighbour.updateDistanceAndPreceding(distance, best)

        doSolve(tail.sortBy(_.getBestFirst), best +: explored, elementsToReach, neighboursMap)

  @tailrec
  private def rewind[T <: Valued](current: Data[T], previous: List[T] = Nil): List[T] =
    current.getCurrentDistance match
      case 0 => current.getElement +: previous
      case _ => rewind(current.getPreceding, current.getElement +: previous)