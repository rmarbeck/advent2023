
import scala.io.Source
import scala.util.Random

/**
 * Representation of an undirected graph
 *
 * @param vertices  a list of contracted (merged) vertices
 * @param edges     a list of all edges (vertexFrom, vertexTo)
 */
case class Graph(vertices: Vector[Set[Int]], edges: Vector[(Int, Int)])

/**
 * Karger's algorithm - a randomized contraction algorithm for finding
 * a minimum cut in an undirected graph.
 * (http://en.wikipedia.org/wiki/Karger%27s_algorithm)
 *
 * This is a "naive" implementation as a new graph is created from
 * the old one in each iteration.
 *
 *  Run-time complexity: O(n2)
 */
class MinCutCalculator(verbose: Boolean = false) {
  /**
   * Calculates the minimum cut using Karger's algorithm
   *
   * @param graph
   * @return  the minimum cut in the graph as a result of a large(r) number of iterations
   */
  def calculateMinCut(graph: Graph): Int = {
    val (iter, printStep) = (10000, 10000)

    if (verbose) println(s"Searching for min cut through ${iter} loops")

    implicit val rand = new Random
    var maxVal = contractVerticesAndCountCuts(graph)
    if (verbose) {
      println(s"Found ${maxVal} as a suitable candidate\nSearching further...")
    }
    for i <- 1 to iter if maxVal == 0
    do
      rand.setSeed(i)
      if (verbose & (i % printStep == 0)) println(s"Loop ${i}\tmin cut: ${maxVal}")
      val cuts = contractVerticesAndCountCuts(graph)
      if (maxVal < cuts) {
        maxVal = cuts
        if (verbose) println(s"Loop ${i} found a new minimum of ${maxVal} cuts")
      }

    maxVal
  }

  private def contractVerticesAndCountCuts(graph: Graph)(implicit rand:Random): Int = {
    graph.vertices.size match
      case value if value == 2 =>
        val cuts = graph.edges count {
          // Only count the edges between two standalone vertices (X -- n edges -- Y)
          // Vertices which have already been contracted do not count!
          case (x, y) => (graph.vertices.head contains x) && !(graph.vertices.head contains y) || (graph.vertices.head contains y) && !(graph.vertices.head contains x)
        }
        if (cuts == 3)
          println(s"${graph.vertices.head.size} - ${graph.vertices.drop(1).head.size} : ${graph.vertices.head.size*graph.vertices.drop(1).head.size} ")
          graph.vertices.head.size*graph.vertices.drop(1).head.size
        else
          0
      case value if value < 2 => 0
      case _ =>
        def indexOf(vertex: Int): Int = {
          graph.vertices indexWhere (_ contains vertex)
        }

        // Take a random edge (v1, v2)
        val (v1, v2) = graph.edges(rand.nextInt(graph.edges.size))
        val (v1_index, v2_index) = (indexOf(v1), indexOf(v2))

        // Contract v2 into v1
        val contractedVertex = graph.vertices(v1_index) ++ graph.vertices(v2_index)
        val newVertices = graph.vertices
          .updated(v1_index, contractedVertex)                 // creates new Vector with Set having both v1 an v2
          .patch(from = v2_index, other = Nil, replaced = 1)   // removes the contracted vertex

        // Filter out any loops
        val newEdges = graph.edges.filterNot {
          case (from, to) =>                                   // removes the edge of the contracted vertex
            (contractedVertex contains from) && (contractedVertex contains to)
        }
        // construct a new graph and iterate
        contractVerticesAndCountCuts(Graph(newVertices, newEdges))
  }
}