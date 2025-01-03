import scala.annotation.{tailrec, targetName}

case class Vertice(id: Int, generations: List[String]):
  def mergeIn(otherVertice: Vertice): Vertice =
    this.copy(generations = otherVertice.generations ::: generations)

object Vertice:
  def apply(asString: String): Vertice = new Vertice(asString.hashCode, List(asString))

case class Edge(vertices: Set[Vertice], weight: Int):
  def onlyOneIsIn(toLookIn: Set[Vertice]): Boolean = toLookIn(vertices.head) ^ toLookIn(vertices.last)
  def bothIn(toLookIn: Set[Vertice]): Boolean = toLookIn(vertices.head) && toLookIn(vertices.last)
  def theOnlyOneIn(toLookIn: Set[Vertice]): Option[Vertice] =
    Seq(Option.when(toLookIn(vertices.head))(vertices.head), Option.when(toLookIn(vertices.last))(vertices.last)).flatten match
      case head :: Nil => Some(head)
      case _ => None

  def contains(vertice: Vertice): Boolean = vertices.contains(vertice)
  def contains(vertice1: Vertice, vertice2: Vertice): Boolean = contains(vertice1) && contains(vertice2)

  @targetName("add")
  def +(other: Edge): Edge =
    require(vertices == other.vertices)
    this.copy(weight = weight + other.weight)

class Graph(val edges: Set[Edge]):
  lazy val optimizedEdges: Map[Vertice, Map[Vertice, Int]] =
    edges.flatMap:
      edge => Seq(edge.vertices.head -> Map(edge.vertices.last -> edge.weight), edge.vertices.last -> Map(edge.vertices.head -> edge.weight))
    .groupMapReduce(_._1)(_._2)(_ ++ _)

  val vertices: Set[Vertice] = edges.flatMap(_.vertices)
  val length: Int = edges.size

  def weightOf(vertice: Vertice): Int =
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


@tailrec
def minCut(graph: Graph): (Int, Int) =
  val vertice = graph.vertices.head
  minCutPhase(graph, vertice) match
    case (last, 3, reducedGraph) =>
      (3, last.generations.length)
    case (_, cutOffPhase, reducedGraph) =>
      minCut(reducedGraph)

def minCutPhase(graph: Graph, vertice: Vertice): (Vertice, Int, Graph) =
  @tailrec
  def findLastAdded(inA: Seq[Vertice], notInA: Set[Vertice]): (Vertice, Vertice) =
    def findVertice: Vertice =
      import collection.parallel.CollectionConverters.IterableIsParallelizable
      if (inA.size > notInA.size)
        val inASet = inA.toSet
        notInA.view.par.map:
          currentNotInA =>
            (currentNotInA, graph.optimizedEdges(currentNotInA).view.filter((k, v) => inASet.contains(k)).values.sum)
        .maxBy(_._2)._1
      else
        inA.view.par.flatMap:
          currentInA => graph.optimizedEdges(currentInA).view.filter((k, v) => notInA.contains(k))
        .toList.groupMapReduce(_._1)(_._2)(_ + _).maxBy(_._2)._1

    notInA.isEmpty match
      case true => (inA.head, inA.tail.head)
      case false =>
        val verticeToAdd = findVertice
        findLastAdded(verticeToAdd +: inA, notInA - verticeToAdd)

  val (last, justBefore) = findLastAdded(Seq(vertice), graph.vertices - vertice)
  (last, graph.weightOf(last), graph.merge(last, justBefore))