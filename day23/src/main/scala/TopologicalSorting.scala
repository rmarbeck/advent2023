
class Heap:
  var values = List[Summit]()
  def push(summit: Summit): Unit = values = summit +: values

  override def toString: String = values.map(_.name).mkString(";")

class Visited:
  var values = List[Summit]()
  def add(summit: Summit): Unit = values = summit +: values
  def contains(summit: Summit): Boolean = values.contains(summit)

  override def toString: String = values.mkString(";")


class WithData[T](element: T, private var currentDistance: Long = Long.MinValue):
  def getElement = element
  def getCurrentDistance: Long = currentDistance
  def updateDistance(newDistance: Long): Unit =
    currentDistance = newDistance

  override def toString: String = s"{$element, $getCurrentDistance}"

def countPart2(element: Summit, visited: List[Summit], result: Summit)(using summitsHolder: SummitsHolder): Option[(Option[Long], List[Summit])] =
      summitsHolder.nextOf(element).filterNot(current => visited.map(_.name).contains(current.name)) match
        case Nil =>
          element.name == result.name match
            case true => Some(Some(0l), List(element))
            case false => None
        case value =>
          val validResults = value.flatMap:
            current =>
              val deepResult = countPart2(current, element +: visited , result)
              deepResult match
                case Some(Some(value), currentList) => Some(Some(value + summitsHolder.distanceBetween(element, current)), element +: currentList)
                case None => None
          validResults.maxByOption(_._1.get)

def count(elementsSorted: List[WithData[Summit]])(using summitsHolder: SummitsHolder): Long =
  elementsSorted match
    case Nil => throw Exception("Not supported")
    case head :: Nil => head.getCurrentDistance
    case head :: tail =>
      val withDataElements = summitsHolder.nextOf(head.getElement).map:
        current => elementsSorted.find(_.getElement == current).foreach:
          withDataElement =>
            val newDistance = head.getCurrentDistance + summitsHolder.distanceBetween(head.getElement, withDataElement.getElement)
            if ( newDistance > withDataElement.getCurrentDistance)
              withDataElement.updateDistance(newDistance)
      count(tail)


def topologicalSort(staringBy: Summit)(using summitsHolder: SummitsHolder): Heap =
  val heap = new Heap
  val visited = new Visited
  topologicalSort(List(staringBy), visited, heap)
  heap

def topologicalSort(elements: List[Summit], visitedElements: Visited, heap: Heap)(using summitsHolder: SummitsHolder): Unit =
  elements match
    case Nil => ()
    case head :: tail =>
      val nonVisited = summitsHolder.nextOf(head).filterNot(visitedElements.contains)
      visitedElements.add(head)
      nonVisited.foreach:
        current => topologicalSort(List(current), visitedElements, heap)
      heap.push(head)