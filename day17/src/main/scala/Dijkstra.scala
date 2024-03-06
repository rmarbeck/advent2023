import scala.annotation.tailrec
import scala.collection.mutable

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
  def index1: Int
  def index2: Int
  def getValue: Int
  def hashed: Int
  def cannotBeat(other: A): Boolean

case class Summit(row: Int, col: Int, value: Int, lastDir: Direction, lastDirCounter: Int, height: Int) extends Valued[Summit]:
  override def index1: Int = row + (col * height)
  override def index2: Int = lastDir.ordinal + (lastDirCounter * Direction.values.length)
  override def getValue: Int = -(row+1)*(col+1)

  override lazy val hashed: Int = hashCode()

  override def cannotBeat(other: Summit): Boolean =
    other match
      case Summit(otherRow, otherCol, _ , otherLastDir , otherLastDirCounter, _) if otherRow == this.row && otherCol == this.col && otherLastDir == this.lastDir => otherLastDirCounter <= this.lastDirCounter
      case _ => (this.row + this.col + 8) < (other.row + other.col)

class GraphFromArray(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit]): Long = second.getElement.value
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Map[Summit, Data[Summit]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    possibleNeighbours.flatMap(potentialNeighbours.get)
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: Array[Array[Data[Summit]]]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    possibleNeighbours.map(currentSummit => potentialNeighbours(currentSummit.index1)(currentSummit.index2))


trait Graph[T <: Valued[T]]:
  def getElements: Seq[T]
  def getListToExploreInitialisedFromStart(startingFrom: T): Seq[Data[T]] =
    val data = getElements.filterNot(_ == startingFrom).map(Data[T](_))
    Data(startingFrom, 0) +: data
  def weightBetween(first: Data[T], second: Data[T]): Long
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: Map[T, Data[T]]): Seq[Data[T]]
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: Array[Array[Data[T]]]): Seq[Data[T]]

class Data[T <: Valued[T]](element: T, private var currentDistance: Long = Long.MaxValue):
  var explored: Boolean = false
  var isASolution: Boolean = false
  def getElement = element
  def hashed = element.hashed
  private var precedingElement: Option[Data[T]] = None
  def getPreceding: Data[T] = precedingElement.get
  def getCurrentDistance: Long = currentDistance
  def getBestFirst: (Long, Int) = (currentDistance, element.getValue)
  def updateDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    currentDistance = newDistance
    precedingElement = Some(preceding)


  def markAsSolution = isASolution = true

  def cannotBeat(other: Data[T]): Boolean =
    getCurrentDistance > other.getCurrentDistance match
      case true => this.element cannotBeat other.getElement
      case false => false


  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solve[T <: Valued[T]](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): List[T] =
    val toExplore = graph.getListToExploreInitialisedFromStart(startingFrom).toList
    val mapOfSummits = toExplore.map(summitData => summitData.getElement -> summitData).toMap
    doSolve[T](toExplore.toVector.take(1), Nil, elementsToReach, mapOfSummits)(graph)


  def solveOptimized[T <: Valued[T]](graph: Graph[T], startingFrom: T, elementsToReach: List[T]): List[T] =
    val toExplore = graph.getListToExploreInitialisedFromStart(startingFrom).toList
    val heightOnIndex1 = toExplore.map(_.getElement.index1).max
    val heightOnIndex2 = toExplore.map(_.getElement.index2).max
    val allElements = Array.ofDim[Data[T]](heightOnIndex1 + 1, heightOnIndex2 + 1)
    toExplore.foreach:
      current => allElements(current.getElement.index1)(current.getElement.index2) = current

    elementsToReach.map(element => allElements(element.index1)(element.index2).markAsSolution)

    given Ordering[Data[T]] with
      def compare(first: Data[T], second: Data[T]) =
        first.getCurrentDistance.compare(second.getCurrentDistance) match
          case 0 => first.getElement.getValue.compare(second.getElement.getValue) match
            case 0 => first.hashed.compare(second.hashed)
            case value => value
          case value => value

    doSolveOptimized[T](mutable.TreeSet(toExplore.head), None, allElements)(graph)


  @tailrec
  private def doSolveOptimized[T <: Valued[T]](toExplore: mutable.TreeSet[Data[T]], lastExplored: Option[Data[T]], allElements: Array[Array[Data[T]]])(implicit graph: Graph[T]): List[T] =
    toExplore match
      case value if value.isEmpty => rewind(lastExplored.get)
      case _ if lastExplored.fold(false)(_.isASolution) => rewind(lastExplored.get)
      case value =>
        val best = toExplore.head
        val tail = toExplore.tail
        val neighboursUpdated = graph.getNeighboursOfIn(best, allElements).flatMap: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance > distance)
            tail.remove(neighbour)
            neighbour.updateDistanceAndPreceding(distance, best)
            Some(neighbour)
          else
            None

        tail.addAll(neighboursUpdated)

        doSolveOptimized(tail, Some(best), allElements)


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