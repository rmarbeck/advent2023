import scala.annotation.tailrec
import scala.collection.mutable

enum Direction:
  def opposite: Direction = Direction.fromOrdinal((ordinal + 2) % Direction.values.length)
  case Left2Right, Up2Down, Right2Left, Down2Up

export Direction.*

type ArrayOfFour[T <: Valued[T]] = Array[Array[Array[Array[Data[T]]]]]

type Valuer = (Int, Int) => Int

trait Valued[A]:
  def indexes: (Int, Int, Int, Int)
  def getSecondOrderingValue: Int
  def hashed: Int
  def correctedDistance(rawDistance: Long): Long
  def inBetween(other: A): Int
  def to(other: A)(using allElements: Valuer): Seq[Int]

case class Summit(row: Int, col: Int, value: Int, lastDir: Direction, lastDirCounter: Int) extends Valued[Summit]:
  override def indexes: (Int, Int, Int, Int) = (row, col, lastDir.ordinal, lastDirCounter)
  override def getSecondOrderingValue: Int = -(row + 1)*(col + 1)

  override def correctedDistance(rawDistance: Long): Long = rawDistance

  override lazy val hashed: Int = hashCode()

  override def inBetween(other: Summit): Int = math.abs(other.row - this.row) + math.abs(other.col - this.col)

  override def to(other: Summit)(using allElements: Valuer): Seq[Int] =
    other.row == row match
      case true =>
        (col until other.col by (other.col - col).sign).map(allElements(row, _))
      case false =>
        (row until other.row by (other.row - row).sign).map(allElements(_, col))


class GraphFromArray(val elements: Seq[Summit])(validNeighbours: Summit => Seq[Summit]) extends Graph[Summit]:
  override def getElements: Seq[Summit] = elements
  override def weightBetween(first: Data[Summit], second: Data[Summit])(using allElements: ArrayOfFour[Summit]): Long =
    second.getElement.inBetween(first.getElement) match
      case 1 => second.getElement.value
      case value =>
        given Valuer = allElements(_: Int)(_: Int)(0)(0).getElement.value
        second.getElement.to(first.getElement).sum
  override def getNeighboursOfIn(current: Data[Summit], potentialNeighbours: ArrayOfFour[Summit]): Seq[Data[Summit]] =
    val possibleNeighbours = validNeighbours.apply(current.getElement)
    possibleNeighbours.map:
      currentSummit =>
        val indexes = currentSummit.indexes
        potentialNeighbours(indexes(0))(indexes(1))(indexes(2))(indexes(3))


trait Graph[T <: Valued[T]]:
  def getElements: Seq[T]
  def getListToExploreInitialisedFromStart(startingFrom: T): Seq[Data[T]] =
    val data = getElements.filterNot(_ == startingFrom).map(Data[T](_))
    Data(startingFrom, 0) +: data
  def weightBetween(first: Data[T], second: Data[T])(using allElements: ArrayOfFour[T]): Long
  def getNeighboursOfIn(current: Data[T], potentialNeighbours: ArrayOfFour[T]): Seq[Data[T]]

class Data[T <: Valued[T]](element: T, private var currentDistance: Long = Long.MaxValue):
  var explored: Boolean = false
  var isASolution: Boolean = false
  def getElement = element
  def hashed = element.hashed
  private var precedingElement: Option[Data[T]] = None
  def getPreceding: Data[T] = precedingElement.get
  def getCurrentDistance: Long = currentDistance
  def getBestFirst: (Long, Int) = (currentDistance, element.getSecondOrderingValue)
  def updateDistanceAndPreceding(newDistance: Long, preceding: Data[T]): Unit =
    currentDistance = newDistance
    precedingElement = Some(preceding)


  def markAsSolution = isASolution = true
  def markExplored(using allElements: ArrayOfFour[T]) =
    explored = true
    val indexes = this.getElement.indexes
    val nbElements = allElements(indexes(0))(indexes(1))(indexes(2)).length
    (indexes(3) + 1 until nbElements).foreach:
      allElements(indexes(0))(indexes(1))(indexes(2))(_).explored = true



  override def toString: String = s"{$element, $getCurrentDistance}"

object Dijkstra:
  def solveOptimized[T <: Valued[T]](graph: Graph[T], startingFrom: T, elementsToReach: List[T], optimize: Boolean): List[T] =
    val toExplore = graph.getListToExploreInitialisedFromStart(startingFrom).toList
    val widths = toExplore.map(_.getElement.indexes).max
    val allElements = Array.ofDim[Data[T]](widths(0) + 1, widths(1) + 1, widths(2) + 1, widths(3) + 1)
    toExplore.foreach:
      current =>
        val indexes = current.getElement.indexes
        allElements(indexes(0))(indexes(1))(indexes(2))(indexes(3)) = current

    elementsToReach.map:
      element =>
        val indexes = element.indexes
        allElements(indexes(0))(indexes(1))(indexes(2))(indexes(3)).markAsSolution

    given Ordering[Data[T]] with
      def compare(first: Data[T], second: Data[T]) =
        first.getElement.correctedDistance(first.getCurrentDistance).compare(second.getElement.correctedDistance(second.getCurrentDistance)) match
          case 0 => first.getElement.getSecondOrderingValue.compare(second.getElement.getSecondOrderingValue) match
            case 0 => first.hashed.compare(second.hashed)
            case value => value
          case value => value

    doSolveOptimized[T](mutable.TreeSet(toExplore.head), None, optimize)(using graph)(using allElements)


  @tailrec
  private def doSolveOptimized[T <: Valued[T]](toExplore: mutable.TreeSet[Data[T]], lastExplored: Option[Data[T]], optimize: Boolean)(using graph: Graph[T])(using allElements: ArrayOfFour[T]): List[T] =
    toExplore match
      case value if value.isEmpty => rewind(lastExplored.get)
      case _ if lastExplored.fold(false)(_.isASolution) => rewind(lastExplored.get)
      case value =>
        val toExploreFiltered = optimize match
          case true => toExplore.span(_.explored)._2
          case false => toExplore
        val best = toExploreFiltered.head
        val tail = toExploreFiltered.tail
        val neighboursUpdated = graph.getNeighboursOfIn(best, allElements).flatMap: neighbour =>
          val distance = best.getCurrentDistance + graph.weightBetween(best, neighbour)
          if (neighbour.getCurrentDistance > distance)
            tail.remove(neighbour)
            neighbour.updateDistanceAndPreceding(distance, best)
            Some(neighbour)
          else
            None

        tail.addAll(neighboursUpdated)

        if (optimize)
          best.markExplored

        doSolveOptimized(tail, Some(best), optimize)

  @tailrec
  private def rewind[T <: Valued[T]](current: Data[T], previous: List[T] = Nil): List[T] =
    current.getCurrentDistance match
      case 0 => current.getElement +: previous
      case _ => rewind(current.getPreceding, current.getElement +: previous)