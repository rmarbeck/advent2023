import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Map}

type Lookup = Map[Position, Id]
type Graph = Map[Id, Connections]
type WeightUnit = Int
type Id = Int
type Target = Id

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    given trailsMap: TrailsMap = TrailsMap(inputLines)

    val crossRoads: Vector[CrossRoad] =
      (trailsMap.start +: findInnerCrossRoads :+ trailsMap.end).zipWithIndex.map((pos, index) => CrossRoad(index, pos))

    given lookup: Lookup = crossRoads.map(cr => cr.position -> cr.id).toMap

    val graph: Graph = crossRoads.filterNot(_.position == trailsMap.end).map:
      cr => cr.id -> Connections.from(distanceFrom(cr))
    .toMap

    val start = 0
    given Target = crossRoads.map(_.id).max

    val result1 =
      given Graph = graph
      s"${longestHike(start)}"

    val result2 =
      given Graph = transformReflexive(graph)
      s"${longestHike(start)}"

    (s"$result1", s"$result2")


def transformReflexive(graph: Graph): Graph =
  graph.map:
    case (id, connections) =>
      val others = graph.filterNot(_._1 == id)
      val updatedConnections = others.foldLeft(connections):
        case (acc, target -> targetConnections) if targetConnections.destinations.contains(id) => acc + (target, targetConnections.weights(id))
        case (acc, _)  => acc
      id -> updatedConnections

def longestHike(from: Id, alreadySeen: BitSet = BitSet(), totalDist: Int = 0)(using graph: Graph, target: Target): Int =
  if (from == target)
    totalDist
  else
    val Connections(connected, distances) = graph(from)
    (connected diff alreadySeen).fold(0):
      case (longest, key) => longest.max(longestHike(key, alreadySeen + from, totalDist + distances(key)))

def ignoreSlopes(input: Seq[String]): Seq[String] =
  def slopeAsNormalPath(char: Char): Char =
    char match
      case '#' => '#'
      case _ => '.'

  input.map(_.map(slopeAsNormalPath))

def findInnerCrossRoads(using maze: TrailsMap): Vector[Position] =
  for
    x <- maze.xIndices; y <- maze.yIndices if maze.isACrossroad(x,y)
  yield
    Position(x, y)
.toVector

def distanceFrom(crossRoad: CrossRoad)(using maze: TrailsMap, lookup: Lookup): List[(Position, Int)] =
  @tailrec
  def walk(beforeAndCurrent: (Position, Position), distance: Int = 1): (Position, Int) =
    val (before, current) = beforeAndCurrent
    if lookup.contains(current) then
      (current, distance)
    else
      val next = maze.next(current).filterNot(_ == before).head
      walk((current, next), distance + 1)

  crossRoad.next.map(walk(_)).toList

case class Connections(destinations: BitSet, weights: Map[Id, WeightUnit]):
  def +(id: Id, weightUnit: WeightUnit): Connections = Connections(destinations + id, weights + (id -> weightUnit))

object Connections:
  def from(connectedCrossRoads: List[(Position, Int)])(using lookup: Lookup): Connections =
    val weights = connectedCrossRoads.map:
      case (position, weight) => (lookup(position), weight)
    .toMap
    Connections(BitSet.fromSpecific(weights.keySet), weights)

case class TrailsMap(input: Seq[String]):
  import TrailsMap.{moves, dirs}
  private val data: Array[Array[Char]] = input.map(_.toCharArray).toArray.transpose
  lazy val xIndices: Range = data.indices
  lazy val yIndices: Range = data(0).indices
  private lazy val (width, height) = (xIndices.end, yIndices.end)

  val start: Position = Position(1, 0)
  val end: Position = Position(width - 2, height - 1)

  private def isDefined(x: Int, y: Int): Boolean = data.isDefinedAt(x) && data(x).isDefinedAt(y)

  def next(position: Position): List[Position] =
    val Position(x, y) = position
    moves.zip(dirs).map:
      case ((dx, dy), authorized) => (x + dx, y + dy, authorized)
    .withFilter((nx, ny, authorized) => isDefined(nx, ny) && List('.', authorized).contains(data(nx)(ny)))
    .map((nx, ny, _) => Position(nx, ny))

  def isACrossroad(x: Int, y: Int): Boolean =
    data(x)(y) match
      case '#' => false
      case _ => moves.map((dx, dy) => (x + dx, y + dy)).count((nx, ny) => isDefined(nx, ny) && data(nx)(ny) != '#') >= 3

object TrailsMap:
  private val moves = List((0, 1), (1, 0), (0, -1), (-1, 0))
  private val dirs = List('v', '>', '^', '<')

case class Position(x: Int, y: Int)

case class CrossRoad(id: Id, position: Position):
  def next(using maze: TrailsMap): Seq[(Position, Position)] = maze.next(position).map(nextPos => (position, nextPos))