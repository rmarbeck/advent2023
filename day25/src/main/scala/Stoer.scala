import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.{BitSet, TreeSet}

case class Vertice(id: Int, generations: Int):
  def mergeIn(otherVertice: Vertice): Vertice =
    this.copy(generations = otherVertice.generations + generations)

object Vertice:
  def apply(asString: String): Vertice = new Vertice(asString.hashCode, 1)

case class Edge(vertices: Set[Vertice], weight: Long):
  lazy val head : Int = vertices.head.id
  lazy val last : Int = vertices.last.id
  def onlyOneIsIn(toLookIn: Set[Vertice]): Boolean = toLookIn(vertices.head) ^ toLookIn(vertices.last)
  def bothIn(toLookIn: Set[Vertice]): Boolean = toLookIn(vertices.head) && toLookIn(vertices.last)
  def theOnlyOneIn(toLookIn: Set[Vertice]): Option[Vertice] =
    Seq(Option.when(toLookIn(vertices.head))(vertices.head), Option.when(toLookIn(vertices.last))(vertices.last)).flatten match
      case head :: Nil => Some(head)
      case _ => None

  def contains(vertice: Vertice): Boolean = vertices.contains(vertice)
  def contains(vertice1: Vertice, vertice2: Vertice): Boolean = contains(vertice1) && contains(vertice2)

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
  def verticeFromId(id: Int): Vertice = verticesMap(id)
  lazy val optimizedEdges: Map[Int, Connections] =
    edges.flatMap:
      _.toSeqOfMap
    .groupMapReduce(_._1)(_._2)(_ ++ _).map((k, v) => k -> Connections.fromWeights(v))

  val vertices: Set[Vertice] = edges.flatMap(_.vertices)
  val verticesMap: Map[Int, Vertice] = vertices.map(v => v.id -> v).toMap
  val length: Int = vertices.size

  def weightOf(vertice: Vertice): Long =
    edges.toSeq.collect:
      case edge@ Edge(_, weight) if edge.contains(vertice) => weight
    .sum
  def merge(verticeToMerge: Vertice, verticeToMergeIn: Vertice): Graph =
    val (untouchedEdges, touched) = edges.partition(edge => !edge.contains(verticeToMerge) && !edge.contains(verticeToMergeIn))
    val newVertice = verticeToMerge.mergeIn(verticeToMergeIn)
    val merged =
      (touched.toSeq.view.collect:
        case edge@ Edge(_, weight) if edge.contains(verticeToMerge) && !edge.contains(verticeToMergeIn) => Edge(edge.vertices.filterNot(_ == verticeToMerge) + newVertice, weight)
        case edge@ Edge(_, weight) if edge.contains(verticeToMergeIn) && !edge.contains(verticeToMerge) => Edge(edge.vertices.filterNot(_ == verticeToMergeIn) + newVertice, weight)
        ).groupMapReduce(_.vertices)(identity)(_ + _).values
    Graph(untouchedEdges ++ merged.toSet)



case class Weight(id: Int, value: Long):
  def toTuple: (Int, Long) = (id, value)
  def toTuple2: (Int, Weight) = (id, this)
  def +(other: Weight): Weight =
    this.copy(value = this.value + other.value)

object Weight:
  def fromTuple(tuple: (Int, Long)): Weight = Weight(tuple._1, tuple._2)

  given orderingWeight: Ordering[Weight] = Ordering.by(w => (-w.value, w.id))

@tailrec
def minCut(graph: Graph): (Int, Int) =
  val vertice = graph.vertices.head
  minCutPhase(graph, vertice) match
    case (last, 3L, reducedGraph) =>
      (3, last.generations)
    case (_, cutOffPhase, reducedGraph) =>
      minCut(reducedGraph)

extension [A](self: TreeSet[A])
  def mergeWith(newMap: Map[Int, A], withConstraint: BitSet): TreeSet[A] = ???


def minCutPhase(graph: Graph, vertice: Vertice): (Vertice, Long, Graph) =
  @tailrec
  def findLastAdded(inA: Seq[Int], inABitSet: BitSet, weights: TreeSet[Weight]): (Vertice, Vertice) =
    inA.size == graph.length match
      case true => (graph.verticeFromId(inA.head), graph.verticeFromId(inA.tail.head))
      case false =>
        val lastAddedInA = inA.head
        val newWeights = TreeSet(
          (graph.optimizedEdges(lastAddedInA).onlyNotIN(inABitSet) ++ weights.view)
            .groupMapReduce(_.id)(identity)(_ + _).values.toSeq:_*)
        val newBest = newWeights.head.id
        findLastAdded(newBest +: inA, inABitSet + newBest, newWeights.tail)

  //val before = System.currentTimeMillis()
  val (last, justBefore) = findLastAdded(Seq(vertice.id), BitSet() + vertice.id, TreeSet())
  //println(s"Time elapsed: ${System.currentTimeMillis() - before}ms")
  (last, graph.weightOf(last), graph.merge(last, justBefore))