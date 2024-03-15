import scala.annotation.tailrec
import scala.collection.immutable.BitSet

class Heap:
  var values = List[Int]()
  def push(summitIndex: Int): Unit = values = summitIndex +: values

class Visited:
  var values = BitSet()
  def add(summitIndex: Int): Unit = values = values + summitIndex
  def contains(summitIndex: Int): Boolean = values.contains(summitIndex)

class WithData[T](element: T, private var currentDistance: Long = Long.MinValue):
  def getElement = element
  def getCurrentDistance: Long = currentDistance
  def updateDistance(newDistance: Long): Unit =
    currentDistance = newDistance

def countDFS(from: Int, to: Int)(using summitsHolder: SummitsHolder): Option[Long] =
  countDFS(from, BitSet(), to)

def countDFS(element: Int, visited: BitSet, result: Int)(using summitsHolder: SummitsHolder): Option[Long] =
      summitsHolder.nextOf(element).filterNot(visited.contains) match
        case Nil =>
          element == result match
            case true => Some(0l)
            case false => None
        case value =>
          val validResults = value.flatMap:
            current =>
              val deepResult = countDFS(current, visited + element , result)
              deepResult match
                case Some(value) => Some(value + summitsHolder.distanceBetween(element, current))
                case _ => None
          validResults.maxOption

@tailrec
def countTopologicallySorted(elementsSorted: List[WithData[Int]])(using summitsHolder: SummitsHolder): Long =
  elementsSorted match
    case Nil => throw Exception("Not supported")
    case head :: Nil => head.getCurrentDistance
    case head :: tail =>
      val withDataElements = summitsHolder.nextOf(head.getElement).map:
        current => elementsSorted.find(_.getElement == current).foreach:
          withDataElement =>
            val newDistance = head.getCurrentDistance + summitsHolder.distanceBetween(head.getElement, withDataElement.getElement)
            if (newDistance > withDataElement.getCurrentDistance)
              withDataElement.updateDistance(newDistance)
      countTopologicallySorted(tail)


def topologicalSort(startingBy: Int)(using summitsHolder: SummitsHolder): List[WithData[Int]] =
  val heap = new Heap
  val visited = new Visited
  topologicalSort(List(startingBy), visited, heap)
  heap.values.zipWithIndex.map:
    case (currentSummit, 0) =>
      WithData[Int](currentSummit, 0)
    case (currentSummit, _) =>
      WithData[Int](currentSummit)

def topologicalSort(elements: List[Int], visitedElements: Visited, heap: Heap)(using summitsHolder: SummitsHolder): Unit =
  elements match
    case Nil => ()
    case head :: tail =>
      val nonVisited = summitsHolder.nextOf(head).filterNot(visitedElements.contains)
      visitedElements.add(head)
      nonVisited.foreach:
        current => topologicalSort(List(current), visitedElements, heap)
      heap.push(head)