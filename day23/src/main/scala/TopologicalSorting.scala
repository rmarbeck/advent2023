
class Heap:
  var values = List[Summit]()
  def push(summit: Summit): Unit = values = summit +: values

  override def toString: String = values.map(_.name).mkString(";")

class Visited:
  var values = List[Summit]()
  def add(summit: Summit): Unit = values = summit +: values
  def contains(summit: Summit): Boolean = values.contains(summit)

  override def toString: String = values.mkString(";")

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