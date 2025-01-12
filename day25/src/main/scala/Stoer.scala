import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.{BitSet, TreeSet}

case class Vertex(id: Int, generations: Int = 1):
  def mergeIn(otherVertex: Vertex): Vertex =
    this.copy(generations = otherVertex.generations + generations)

object Vertex:
  def apply(asString: String): Vertex = new Vertex(asString.hashCode, 1)

case class Edge(vertices: Set[Vertex], weight: Long):
  lazy val head : Int = vertices.head.id
  lazy val last : Int = vertices.last.id

  def contains(vertex: Vertex): Boolean = vertices.contains(vertex)

  def toSeqOfMap: Seq[(Int,Map[Int, Long])] = Seq(head -> Map(last -> weight), last -> Map(head -> weight))

  @targetName("add")
  def +(other: Edge): Edge =
    this.copy(weight = weight + other.weight)


case class Connections(verticesTo: BitSet, weights: Map[Int, Weight]):
  def onlyNotIN(exclusion: BitSet): scala.collection.View[Weight] =
    val toTake = verticesTo diff exclusion
    toTake.map(key => weights(key)).view

object Connections:
  def fromWeights(weights: Map[Int, Long]): Connections =
    val verticesTo = BitSet.fromSpecific(weights.keys)
    new Connections(verticesTo, weights.map((k, v) => k -> Weight(k, v)))

class Graph(val edges: Set[Edge]):
  def verticesFromId(id: Int): Vertex = verticesMap(id)
  lazy val optimizedEdges: Map[Int, Connections] =
    edges.flatMap:
      _.toSeqOfMap
    .groupMapReduce(_._1)(_._2)(_ ++ _).map((k, v) => k -> Connections.fromWeights(v))

  val vertices: Set[Vertex] = edges.flatMap(_.vertices)
  private val verticesMap: Map[Int, Vertex] = vertices.map(v => v.id -> v).toMap
  val length: Int = vertices.size

  def weightOf(vertex: Vertex): Long =
    edges.toSeq.collect:
      case edge@ Edge(_, weight) if edge.contains(vertex) => weight
    .sum
  def merge(vertexToMerge: Vertex, vertexToMergeIn: Vertex): Graph =
    val (untouchedEdges, touched) = edges.partition(edge => !edge.contains(vertexToMerge) && !edge.contains(vertexToMergeIn))
    val newVertex = vertexToMerge.mergeIn(vertexToMergeIn)
    val merged =
      (touched.toSeq.view.collect:
        case edge@ Edge(_, weight) if edge.contains(vertexToMerge) && !edge.contains(vertexToMergeIn) => Edge(edge.vertices.filterNot(_ == vertexToMerge) + newVertex, weight)
        case edge@ Edge(_, weight) if edge.contains(vertexToMergeIn) && !edge.contains(vertexToMerge) => Edge(edge.vertices.filterNot(_ == vertexToMergeIn) + newVertex, weight)
        ).groupMapReduce(_.vertices)(identity)(_ + _).values
    Graph(untouchedEdges ++ merged.toSet)

case class Weight(id: Int, value: Long)

object Weight:
  def fromTuple(tuple: (Int, Long)): Weight = Weight(tuple._1, tuple._2)
  given orderingWeight: Ordering[Weight] = Ordering.by(w => (-w.value, w.id))

@tailrec
def minCut(graph: Graph): (Int, Int) =
  val a = graph.vertices.head
  minCutPhase(graph, a) match
    case (last, 3L, reducedGraph) => (3, last.generations)
    case (_, cutOffPhase, reducedGraph) => minCut(reducedGraph)

case class WeightsHeap(weights: Map[Int, Long], heap: TreeSet[Weight]):
  inline def add(newWeight: Weight): WeightsHeap =
    val Weight(id, weight) = newWeight
    weights.get(id) match
      case Some(existingWeight) =>
        WeightsHeap(weights + (id -> (existingWeight + weight)), heap - Weight(id, existingWeight) + Weight(id, existingWeight + weight))
      case _ =>
        WeightsHeap(weights + (id -> weight), heap + newWeight)

  lazy val head: Weight = heap.head
  lazy val tail: WeightsHeap = WeightsHeap(weights - head.id, heap.tail)

object WeightsHeap:
  val empty: WeightsHeap = WeightsHeap(Map.empty, TreeSet.empty)

type LastTwo = (Vertex, Vertex)

def minCutPhase(graph: Graph, vertex: Vertex): (Vertex, Long, Graph) =
  @tailrec
  def findLastAdded(inA: Seq[Int], inABitSet: BitSet, heap: WeightsHeap): LastTwo =
    inA.size == graph.length match
      case true => (graph.verticesFromId(inA.head), graph.verticesFromId(inA(1)))
      case false =>
        val newHeap =
          graph.optimizedEdges(inA.head).onlyNotIN(inABitSet).foldLeft(heap):
            case (acc, weight) => acc.add(weight)

        val (head, tail) = (newHeap.head.id, newHeap.tail)
        findLastAdded(head +: inA, inABitSet + head, tail)

  val (last, justBefore) = findLastAdded(Seq(vertex.id), BitSet(vertex.id), WeightsHeap.empty)
  (last, graph.weightOf(last), graph.merge(last, justBefore))