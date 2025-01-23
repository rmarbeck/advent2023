import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, TreeSet, Map as iMap}
import scala.collection.mutable
import scala.collection.mutable.Map

type Dimension = Int
type Target = (Int, Int)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result1 = 0
      given Context = Context.fromInputPart1(inputLines)
      val edges = findVertices(List(VerticesPath.init), BitSet(), Set(Vertex(1, Summit(1, 0)), Vertex(0, Summit.target)), Set(), Set())._2
      given graph: Graph = Graph(edges)
      println(solverExp(TreeSet(NewPath(0, graph.start, BitSet())), graph.target.summit, None))


    val result2 =
      given Context = Context.fromInputPart2(inputLines)
      val edges = findVertices(List(VerticesPath.init), BitSet(), Set(Vertex(1, Summit(1, 0)), Vertex(0, Summit.target)), Set(), Set())._2
      //edges.toList.sortBy(_.lowestId).foreach(println)
      given graph: Graph = Graph(edges)

      println(solverExp(TreeSet(NewPath(0, graph.start, BitSet())), graph.target.summit, None))


    (s"$result1", s"$result2")

type Id = Int
type WeightUnit = Int

case class Vertex(id: Id, summit: Summit)

case class Edge(vertices: Set[Vertex], weight: WeightUnit):
  lazy val lowestId: Id = math.min(head, last)
  lazy val head : Id = vertices.head.id
  lazy val last : Id = vertices.last.id

  def toSeqOfMap: Seq[(Id,Seq[(Id, WeightUnit)])] = Seq(head -> Seq(last -> weight), last -> Seq(head -> weight))

object Edge:
  def apply(vertex1: Vertex, vertex2: Vertex, weight: WeightUnit): Edge = new Edge(Set(vertex1, vertex2), weight)

case class VerticesPath(distance: Int, currentSummit: Summit, lastVertex: Vertex)(using context: Context):
  lazy val summitId: Int = currentSummit.asInt
  def shortcut: Boolean = lastVertex.summit == currentSummit
  def newVertex(vertex: Vertex): VerticesPath = this.copy(distance = 0, lastVertex = vertex)
  def walk: VerticesPath = this.copy(distance = distance + 1)
  def next: Seq[VerticesPath] =
    context.grid.nextCached(currentSummit).map(summit => VerticesPath(distance+1, summit, lastVertex))

object VerticesPath:
  def init(using Context): VerticesPath = VerticesPath(-1, Summit(1, 0), Vertex(1, Summit(1, 0)))

@tailrec
def findVertices(toExplore: List[VerticesPath], explored: BitSet, vertices: Set[Vertex], edgesPart1: Set[Edge], edgesPart2: Set[Edge])(using context:Context): (Set[Edge], Set[Edge]) =
  toExplore match
    case Nil => (edgesPart1, edgesPart2)
    case head :: tail if explored.contains(head.summitId) => findVertices(tail, explored, vertices, edgesPart1, edgesPart2)
    case head :: tail =>
      val next = head.next

      next.filterNot(path => explored.contains(path.summitId)).toList match
        case Nil =>
          val pathThroughVertex = next.filter(path => vertices.map(_.summit).contains(path.currentSummit)).filterNot(_.shortcut)
          val (newEdgesPart1, newEdgesPart2) = pathThroughVertex match
            case onlyOne :: Nil =>
              (edgesPart1 + Edge(onlyOne.lastVertex, vertices.find(_.summit == onlyOne.currentSummit).get, onlyOne.distance + 1)
                ,
                edgesPart2 + Edge(onlyOne.lastVertex, vertices.find(_.summit == onlyOne.currentSummit).get, onlyOne.distance + 1))
            case _ =>
              if (vertices.find(_.id == 0).map(_.summit).contains(head.currentSummit))
                (edgesPart1 + Edge(head.lastVertex, vertices.find(_.summit == head.currentSummit).get, head.distance + 1),
                  edgesPart2 + Edge(head.lastVertex, vertices.find(_.summit == head.currentSummit).get, head.distance + 1))
              else
                (edgesPart1, edgesPart2)

          findVertices(tail, explored + head.summitId, vertices, newEdgesPart1, newEdgesPart2)
        case onlyOne :: Nil => findVertices(onlyOne :: tail, explored + head.summitId, vertices, edgesPart1, edgesPart2)
        case several =>
          val newVertex = Vertex(vertices.size, head.currentSummit)
          val newEdgesPart1 = edgesPart1 + Edge(head.lastVertex, newVertex, head.distance + 1)
          val newEdgesPart2 = edgesPart2 + Edge(head.lastVertex, newVertex, head.distance + 1)
          //println(s"here : ${Edge(head.lastVertex, newVertex, head.distance + 1)}")
          findVertices(several.map(_.newVertex(newVertex)) ::: tail, explored + head.summitId, vertices + newVertex, newEdgesPart1, newEdgesPart2)

class Graph(edges: Set[Edge]):
  val lookup: iMap[Id, Vertex] = edges.flatMap(_.vertices).map(vertex => vertex.id -> vertex).toMap
  private val optimizedEdges: iMap[Id, Seq[(Id, WeightUnit)]] =
    val test = edges.flatMap(_.toSeqOfMap)
    val test2 = test.groupMapReduce(_._1)(_._2)(_ ++ _)
    test2
  val start: Vertex = lookup(1)
  val target: Vertex = lookup(0)
  /*println("******")
  println(lookup)
  println(edges)
  println(s"sum : ${edges.map(_.weight).sum}")
  println(optimizedEdges(0).map((id, weight) => (lookup(id), weight)))
  println("------")*/
  def isConnectedToTarget(vertex: Vertex): Boolean = optimizedEdges(vertex.id).map(_._1).contains(0)
  def connectedVertex(vertex: Vertex): Seq[(Vertex, WeightUnit)] = optimizedEdges(vertex.id).map((id, weight) => (lookup(id), weight))


case class NewPath(distance: Int, currentVertex: Vertex, vertices: BitSet, path: String = "")(using graph: Graph):
  lazy val connectedToTarget: Int = if graph.isConnectedToTarget(currentVertex) then 1 else 0
  private lazy val distanceToTarget = currentVertex.summit.distanceToTarget
  def next: Seq[NewPath] =
    graph.connectedVertex(currentVertex).filterNot(nextVertex => vertices.contains(nextVertex._1.id)).map:
      case (vertex, weight) => NewPath(distance + weight, vertex, vertices + currentVertex.id, path/*s"${currentVertex.summit} - (${distance + weight}), ${path}"*/)

object NewPath:
  given ordering: Ordering[NewPath] =
    given Ordering[BitSet] = BitsetOrdering.updateBitSet
    Ordering.by(p => (-p.distanceToTarget, -p.distance,  p.vertices))

@tailrec
def solverExp(toExplore: TreeSet[NewPath], toReach: Summit, found: Option[Int] = None)(using graph: Graph): Option[Int] =
  toExplore match
    case empty if empty.isEmpty => None
    case notEmpty =>
      notEmpty.head match
        case NewPath(distance, summit, vertices, path) if summit.summit == toReach =>
          //println(path)
          Some(distance)
        case head =>
          solverExp(toExplore.tail ++ head.next, toReach, found)



case class Context(grid: Grid, dimension: Dimension, target: Target)

object Context:
  private def from(arrayOfVals: Array[Array[Char]]): Context =
    val grid = new Grid(arrayOfVals)
    val dimension = grid.size
    new Context(grid, dimension, (dimension - 2, dimension - 1))

  private def slopeAsNormalPath(char: Char): Char =
    char match
      case '#' => '#'
      case _ => '.'

  def fromInputPart1(inputLines: Seq[String]): Context = from(inputLines.map(_.toCharArray).toArray.transpose)
  def fromInputPart2(inputLines: Seq[String]): Context = from(inputLines.map(_.toCharArray.map(slopeAsNormalPath)).toArray.transpose)



class Grid(private val arrayOfVals: Array[Array[Char]]):
  def getSummits(using context:Context): Seq[Summit] =
    for
      x <- arrayOfVals.indices
      y <- arrayOfVals(x).indices
      if arrayOfVals(x)(y) != '#'
    yield
      Summit(x, y)

  lazy val size: Int = arrayOfVals.length
  val cachedNext : mutable.Map[Summit, Seq[Summit]] = mutable.Map.empty

  def nextCached(summit: Summit)(using context:Context): Seq[Summit] =
    cachedNext.getOrElseUpdate(summit, next(summit))

  def next(summit: Summit)(using context:Context): Seq[Summit] =
    val Summit(x, y) = summit
    def nextValid(deltas: Seq[(Int, Int)]): Seq[Summit] =
      deltas.map((dX, dY) => (x+dX, y+dY))
        .withFilter((newX, newY) => arrayOfVals.isDefinedAt(newX) && arrayOfVals(newX).isDefinedAt(newY) && arrayOfVals(newX)(newY) != '#')
        .map((newX, newY) => Summit(newX, newY))

    val atMost = arrayOfVals(x)(y) match
      case '.' =>
        Seq((0, 1), (0, -1), (1, 0), (-1, 0))
      case '>' => Seq((1, 0))
      case '<' => Seq((-1, 0))
      case '^' => Seq((0, -1))
      case 'v' => Seq((0, 1))
      case _ => Seq.empty

    nextValid(atMost)

case class Summit(x: Int, y: Int)(using context:Context):
  def distance(other: Summit) = (other.x - x).abs + (other.y - y).abs
  lazy val asInt: Int = (x + 1)*context.dimension + (y + 1)
  private val (targetX, targetY) = context.target
  lazy val distanceToTarget: Int = (targetX - x).abs + (targetY - y).abs
  def next: Seq[Summit] = context.grid.next(this)

object Summit:
  def target(using context:Context): Summit = Summit(context.target._1, context.target._2)

case class Path(distance: Int, currentSummit: Summit, summits: BitSet)(using context: Context):
  def next: Seq[Path] =
    context.grid.nextCached(currentSummit).withFilter:
      case summit: Summit if summits.contains(summit.asInt) => false
      case _ => true
    .map(summit => Path(distance+1, summit, summits + summit.asInt))


object Path:
  given ordering: Ordering[Path] =
    given Ordering[BitSet] = BitsetOrdering.updateBitSet
    Ordering.by(p => (-p._2.distanceToTarget, -p.distance,  p.summits))
  def from(summit: Summit)(using context:Context): Path = new Path(0, summit, BitSet(summit.asInt))

object BitsetOrdering:
  given updateBitSet: Ordering[BitSet] with
    override def compare(x: BitSet, y: BitSet): Int =
      x == y match
        case true => 0
        case false =>
          x diff y match
            case difference if difference.isEmpty => -1
            case _=> 1

@tailrec
def solver(toExplore: TreeSet[Path], toReach: Summit, found: Option[Int] = None)(using context:Context): Option[Int] =
  toExplore match
    case empty if empty.isEmpty => None
    case notEmpty =>
      notEmpty.head match
        case Path(distance, summit, _) if summit == toReach => Some(distance)
        case head @ Path(distance, _, _) =>
          /*if (head.currentSummit.distanceToTarget < 100)
            println(s"${toExplore.size} => ${notEmpty.head.distance} (${notEmpty.head.currentSummit.distanceToTarget})")
          //println(s"${notEmpty.size} => ${notEmpty.head.distance} (${notEmpty.head.currentSummit.distanceToTarget})")*/
          solver(toExplore.tail ++ head.next, toReach, found)