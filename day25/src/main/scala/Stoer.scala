import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.{BitSet, TreeSet}

case class Vertex(id: Int, generations: Int):
  def mergeIn(otherVertex: Vertex): Vertex =
    this.copy(generations = otherVertex.generations + generations)

object Vertex:
  def apply(asString: String): Vertex = new Vertex(asString.hashCode, 1)

case class Edge(vertices: Set[Vertex], weight: Long):
  lazy val head : Int = vertices.head.id
  lazy val last : Int = vertices.last.id
  def onlyOneIsIn(toLookIn: Set[Vertex]): Boolean = toLookIn(vertices.head) ^ toLookIn(vertices.last)
  def bothIn(toLookIn: Set[Vertex]): Boolean = toLookIn(vertices.head) && toLookIn(vertices.last)
  def theOnlyOneIn(toLookIn: Set[Vertex]): Option[Vertex] =
    Seq(Option.when(toLookIn(vertices.head))(vertices.head), Option.when(toLookIn(vertices.last))(vertices.last)).flatten match
      case head :: Nil => Some(head)
      case _ => None

  def contains(vertex: Vertex): Boolean = vertices.contains(vertex)
  def contains(vertex1: Vertex, vertex2: Vertex): Boolean = contains(vertex1) && contains(vertex2)

  def toSeqOfMap: Seq[(Int,Map[Int, Long])] = Seq(head -> Map(last -> weight), last -> Map(head -> weight))

  @targetName("add")
  def +(other: Edge): Edge =
    this.copy(weight = weight + other.weight)


case class Connections(verticesTo: BitSet, weights: Map[Int, Weight]):
  def onlyNotIN(exclusion: BitSet): scala.collection.View[Weight] =
    val toTake = verticesTo diff(exclusion)
    toTake.map(key => weights(key)).view

object Connections:
  def fromWeights(weights: Map[Int, Long]): Connections =
    val verticesTo = BitSet(weights.keys.toSeq:_*)
    new Connections(verticesTo, weights.map((k, v) => k -> Weight(k, v)))


class Graph(val edges: Set[Edge]):
  def verticesFromId(id: Int): Vertex = verticesMap(id)
  lazy val optimizedEdges: Map[Int, Connections] =
    edges.flatMap:
      _.toSeqOfMap
    .groupMapReduce(_._1)(_._2)(_ ++ _).map((k, v) => k -> Connections.fromWeights(v))

  val vertices: Set[Vertex] = edges.flatMap(_.vertices)
  val verticesMap: Map[Int, Vertex] = vertices.map(v => v.id -> v).toMap
  val length: Int = vertices.size

  def weightOf(vertice: Vertex): Long =
    edges.toSeq.collect:
      case edge@ Edge(_, weight) if edge.contains(vertice) => weight
    .sum
  def merge(verticeToMerge: Vertex, verticeToMergeIn: Vertex): Graph =
    val (untouchedEdges, touched) = edges.partition(edge => !edge.contains(verticeToMerge) && !edge.contains(verticeToMergeIn))
    val newVertice = verticeToMerge.mergeIn(verticeToMergeIn)
    val merged =
      (touched.toSeq.view.collect:
        case edge@ Edge(_, weight) if edge.contains(verticeToMerge) && !edge.contains(verticeToMergeIn) => Edge(edge.vertices.filterNot(_ == verticeToMerge) + newVertice, weight)
        case edge@ Edge(_, weight) if edge.contains(verticeToMergeIn) && !edge.contains(verticeToMerge) => Edge(edge.vertices.filterNot(_ == verticeToMergeIn) + newVertice, weight)
        ).groupMapReduce(_.vertices)(identity)(_ + _).values
    Graph(untouchedEdges ++ merged.toSet)



class Weight(val id: Int, val value: Long):
  def toTuple: (Int, Long) = (id, value)
  def toTuple2: (Int, Weight) = (id, this)
  def +(other: Weight): Weight = Weight(id, this.value + other.value)

  override def hashCode(): Int = id

  override def equals(obj: Any): Boolean =
    println("here")
    obj match
      case that: Weight => id == that.id
      case _ => false

  override def toString: String = s"Weight(${id}, ${value})"

object Weight:
  def fromTuple(tuple: (Int, Long)): Weight = Weight(tuple._1, tuple._2)
  given orderingWeight: Ordering[Weight] = Ordering.by(w => (-w.value, w.id))

@tailrec
def minCut(graph: Graph): (Int, Int) =
  val vertex = graph.vertices.head
  minCutPhase(graph, vertex) match
    case (last, 3L, reducedGraph) =>
      (3, last.generations)
    case (_, cutOffPhase, reducedGraph) =>
      minCut(reducedGraph)

case class WeightsHeap(weights: Map[Int, Long], heap: TreeSet[Weight]):
  def add(newWeight: Weight): WeightsHeap =
    weights.get(newWeight.id) match
      case Some(existingWeight) =>
        WeightsHeap(weights + (newWeight.id -> (existingWeight + newWeight.value)), heap - Weight(newWeight.id, existingWeight) + Weight(newWeight.id, existingWeight + newWeight.value))
      case _ =>
        WeightsHeap(weights + (newWeight.id -> newWeight.value), heap + Weight(newWeight.id, newWeight.value))

  def tail: WeightsHeap =
    val best = heap.head
    WeightsHeap(weights - best.id, heap.tail)

object WeightsHeap:
  val empty: WeightsHeap = WeightsHeap(Map(), TreeSet())


def minCutPhase(graph: Graph, vertex: Vertex): (Vertex, Long, Graph) =
  @tailrec
  def findLastAdded(inA: Seq[Int], inABitSet: BitSet, heap: WeightsHeap): (Vertex, Vertex) =
    inA.size == graph.length match
      case true => (graph.verticesFromId(inA.head), graph.verticesFromId(inA.tail.head))
      case false =>
        val lastAddedInA = inA.head
        val newHeap =
          graph.optimizedEdges(lastAddedInA).onlyNotIN(inABitSet).foldLeft(heap):
            case (acc, weight) => acc.add(weight)

        val newBest = newHeap.heap.head.id
        findLastAdded(newBest +: inA, inABitSet + newBest, newHeap.tail)

  val (last, justBefore) = findLastAdded(Seq(vertex.id), BitSet() + vertex.id, WeightsHeap.empty)
  (last, graph.weightOf(last), graph.merge(last, justBefore))