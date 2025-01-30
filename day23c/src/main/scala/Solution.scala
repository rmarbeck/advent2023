import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Map}

type Lookup = Map[Position, Id]
type Graph = Map[Id, Connections]
type WeightUnit = Int
type Id = Int

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val before = System.currentTimeMillis()
    val result1 = solveFor(inputLines)
    println(s"Time is ${System.currentTimeMillis() - before}ms")

    val result2 = solveFor(ignoreSlopes(inputLines))

    (s"$result1", s"$result2")

def solveFor(input: Seq[String]): String =
  given maze: Maze = Maze(input)

  val crossRoads: Vector[CrossRoad] =
    (maze.start +: findInnerCrossRoads :+ maze.end).zipWithIndex.map((pos, index) => CrossRoad(index, pos))

  given lookup: Lookup = crossRoads.map(cr => cr.position -> cr.id).toMap

  given graph: Graph = crossRoads.filterNot(_.position == maze.end).map:
    cr => cr.id -> Connections.from(distanceFrom(cr))
  .toMap

  val start = crossRoads.map(_.id).min
  val end = crossRoads.map(_.id).max

  s"${search(start, end)}"


def search(vertex: Id, target: Id, vertices: BitSet = BitSet(), totalDist: Int = 0)(using graph: Graph): Int =
  if (vertex == target)
    totalDist
  else
    val Connections(possible, values) = graph(vertex)
    (possible diff vertices).foldLeft(0):
      case (longest, key) => longest.max(search(key, target, vertices + vertex, totalDist + values(key)))

def ignoreSlopes(input: Seq[String]): Seq[String] =
  def slopeAsNormalPath(char: Char): Char =
    char match
      case '#' => '#'
      case _ => '.'

  input.map(_.map(slopeAsNormalPath))

def findInnerCrossRoads(using maze: Maze): Vector[Position] =
    for
      x <- maze.xIndices; y <- maze.yIndices if maze.isACrossroad(x,y)
    yield
      Position(x, y)
  .toVector

def distanceFrom(crossRoad: CrossRoad)(using maze: Maze, lookup: Lookup): List[(Position, Int)] =
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

case class Maze(input: Seq[String]):
  import Maze.{moves, dirs}
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

object Maze:
  private val moves = List((0, 1), (1, 0), (0, -1), (-1, 0))
  private val dirs = List('v', '>', '^', '<')

case class Position(x: Int, y: Int)

case class CrossRoad(id: Id, position: Position):
  def next(using maze: Maze): Seq[(Position, Position)] = maze.next(position).map(nextPos => (position, nextPos))