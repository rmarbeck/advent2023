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

trait Valued[A]:
  def getValue: Int
    def cannotBeat(other: A): Boolean

case class Summit(row: Int, col: Int, value: Int, lastDir: Direction, lastDirCounter: Int) extends Valued[Summit]:
  override def getValue: Int = -(row+1)*(col+1)

  override def cannotBeat(other: Summit): Boolean =
    other match
      case Summit(otherRow, otherCol, _ , otherLastDir , otherLastDirCounter) if otherRow == this.row && otherCol == this.col && otherLastDir == this.lastDir => otherLastDirCounter <= this.lastDirCounter
      case _ => (this.row + this.col + 8) < (other.row + other.col)

class GraphFromArray(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = second.getElement.value
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Map[Summit, Data[Summit]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    possibleNeighbours.flatMap(potentialNeighbours.get)

trait Graph[T <: Valued[T]]:
  def getElements: Seq[T]
  def getListToExploreInitialisedFromStart(startingFrom: T): Seq[Data[T]] =
    val data = getElements.filterNot(_ == startingFrom).map(Data[T](_))
    Data(startingFrom, 0) +: data
  def weightBetween(first: Data[T], second: Data[T]): Long
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: Map[T, Data[T]]): Seq[Data[T]]

class Data[T <: Valued[T]](element: T, private var currentDistance: Long = Long.MaxValue):
  def getElement = element
  private var precedingElement: Option[Data[T]] = None
  def getPreceding: Data[T] = precedingElement.get
  def getCurrentDistance: Long = currentDistance
  def getBestFirst: (Long, Int) = (currentDistance, element.getValue)
  def updateDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    currentDistance = newDistance
    precedingElement = Some(preceding)

  def cannotBeat(other: Data[T]): Boolean =
    getCurrentDistance > other.getCurrentDistance match
      case true => this.element cannotBeat other.getElement
      case false => false


  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solve[T <: Valued[T]](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): List[T] =
    val toExplore = graph.getListToExploreInitialisedFromStart(startingFrom).toList
    val mapOfSummits = toExplore.map(summitData => summitData.getElement -> summitData).toMap
    doSolve[T](toExplore.take(1).toVector, Nil, elementsToReach, mapOfSummits)(graph)

  @tailrec
  private def doSolve[T <: Valued[T]](toExplore: Vector[Data[T]], explored: List[Data[T]], elementsToReach: List[T], neighboursMap : Map[T, Data[T]])(implicit graph: Graph[T]): List[T] =
    toExplore match
      case value if value.isEmpty => rewind(explored.head)
      case _ if  (explored.map(_.getElement) intersect elementsToReach).nonEmpty => println(explored.length); rewind(explored.head)
      case value =>
        val best = toExplore.head
        val tail = toExplore.tail
        //println(best)
        val neighboursUpdated = graph.getNeighboursOfIn(best, neighboursMap).toVector.flatMap: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance > distance)
            neighbour.updateDistanceAndPreceding(distance, best)
            Some(neighbour)
          else
            None

        val potentialNextListToExplore = (neighboursUpdated concat tail).sortBy(_.getBestFirst)
        val futureBest = potentialNextListToExplore.head
        val filtered = potentialNextListToExplore.filterNot(_ cannotBeat futureBest)

        doSolve( filtered, best +: explored, elementsToReach, neighboursMap)

  @tailrec
  private def rewind[T <: Valued[T]](current: Data[T], previous: List[T] = Nil): List[T] =
    current.getCurrentDistance match
      case 0 => current.getElement +: previous
      case _ => rewind(current.getPreceding, current.getElement +: previous)