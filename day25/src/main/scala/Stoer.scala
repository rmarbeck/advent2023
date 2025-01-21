import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.{BitSet, TreeSet}

type Id = Int
type WeightUnit = Int

case class Vertex(id: Id, generations: BitSet):
  def this(id: Id) = this(id, BitSet(id))
  def mergeIn(otherVertex: Vertex): Vertex = otherVertex.copy(generations = otherVertex.generations ++ this.generations)

case class Edge(vertices: Set[Vertex], weight: WeightUnit):
  lazy val head : Id = vertices.head.id
  lazy val last : Id = vertices.last.id

  def toSeqOfMap: Seq[(Id,Map[Id, WeightUnit])] = Seq(head -> Map(last -> weight), last -> Map(head -> weight))

case class Connections(verticesTo: BitSet, weights: Map[Id, Weight]):
  def mergeVertices(vertexToMergeId: Id, vertexToMergeInId: Id): Connections =
    val updatedVertices = verticesTo - vertexToMergeId + vertexToMergeInId
    val updatedConnections =
      val weightOfToMerge = weights(vertexToMergeId)
      val newWeightOfToMergeIn =
        vertexToMergeInId -> (weights.getOrElse(vertexToMergeInId, Weight(vertexToMergeInId, 0)) + weightOfToMerge)

      (weights + newWeightOfToMergeIn) - vertexToMergeId

    Connections(updatedVertices, updatedConnections)

  def mergeConnectionsWith(connectionsToMergeIn: Connections, vertexToMergeId: Id, vertexToMergeInId: Id): Connections =
    val updatedVertices = (verticesTo ++ connectionsToMergeIn.verticesTo) - vertexToMergeInId - vertexToMergeId
    val updatedWeights = weights.foldLeft(connectionsToMergeIn.weights.removed(vertexToMergeId)):
      case (acc, (vertexTo, _)) if vertexTo == vertexToMergeInId => acc
      case (acc, (vertexTo, weight)) if connectionsToMergeIn.verticesTo.contains(vertexTo) =>
        acc + (vertexTo -> (weights(vertexTo) + connectionsToMergeIn.weights(vertexTo)))
      case (acc, (vertexTo, weight)) =>
        acc + (vertexTo -> weights(vertexTo))

    Connections(updatedVertices, updatedWeights)

  def onlyNotIN(exclusion: BitSet): scala.collection.View[Weight] = (verticesTo diff exclusion).view.map(key => weights(key))

object Connections:
  def fromWeights(weights: Map[Int, WeightUnit]): Connections =
    val verticesTo = BitSet.fromSpecific(weights.keys)
    new Connections(verticesTo, weights.map((k, v) => k -> Weight(k, v)))

type OEdges = Map[Id, Connections]
type OVertices = Map[Id, Vertex]

class Graph private(val verticesMap: OVertices, val optimizedEdges: OEdges):
  lazy val vertices: Set[Vertex] = verticesMap.values.toSet
  lazy val length: Int = vertices.size
  def verticesFromId(id: Int): Vertex = verticesMap(id)

  def weightOf(vertex: Vertex): Long = optimizedEdges(vertex.id).weights.values.map(_.value).sum

  def merge(vertexToMerge: Vertex, vertexToMergeIn: Vertex): Graph =
    val newVertex = vertexToMerge.mergeIn(vertexToMergeIn)

    val connectionsOfMerged = optimizedEdges(vertexToMerge.id)
    val connectionsOfMergedIn = optimizedEdges(vertexToMergeIn.id)
    val newConnections = connectionsOfMerged.mergeConnectionsWith(connectionsOfMergedIn, vertexToMerge.id, vertexToMergeIn.id)

    val alteredConnections = connectionsOfMerged.verticesTo - vertexToMergeIn.id

    val updatedOptimizedEdges = alteredConnections.foldLeft(optimizedEdges.removed(vertexToMerge.id)):
      case (acc, vertex) => acc + (vertex -> acc(vertex).mergeVertices(vertexToMerge.id, vertexToMergeIn.id))

    val newOptimizedEdges =  updatedOptimizedEdges + (vertexToMergeIn.id -> newConnections)
    val newMap = verticesMap.removed(vertexToMerge.id) + (newVertex.id -> newVertex)

    Graph(newMap, newOptimizedEdges)

object Graph:
  def fromEdges(edges: Set[Edge]): Graph =
    val verticesMap = edges.flatMap(_.vertices).map(v => v.id -> v).toMap
    val oEdges = edges.flatMap:
      _.toSeqOfMap
    .groupMapReduce(_._1)(_._2)(_ ++ _).map((k, v) => k -> Connections.fromWeights(v))

    Graph(verticesMap, oEdges)

case class Weight(id: Int, value: Long):
  def +(other: Weight): Weight = this.copy(value = this.value + other.value)


object Weight:
  def fromTuple(tuple: (Int, Long)): Weight = Weight(tuple._1, tuple._2)
  given orderingWeight: Ordering[Weight] = Ordering.by(w => (-w.value, w.id))

@tailrec
def minCut(graph: Graph): (Int, Int) =
  val a = graph.vertices.head
  minCutPhase(graph, a) match
    case (last, 3L, reducedGraph) => (3, last.generations.size)
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
    inABitSet.size == graph.length match
      case true => (graph.verticesFromId(inA.head), graph.verticesFromId(inA(1)))
      case false =>
        val newHeap =
          graph.optimizedEdges(inA.head).onlyNotIN(inABitSet).foldLeft(heap):
            case (acc, weight) => acc.add(weight)

        val (head, tail) = (newHeap.head.id, newHeap.tail)
        findLastAdded(head +: inA, inABitSet + head, tail)

  val (last, justBefore) = findLastAdded(Seq(vertex.id), BitSet(vertex.id), WeightsHeap.empty)
  (last, graph.weightOf(last), graph.merge(last, justBefore))