import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, TreeSet}
import scala.collection.mutable
import scala.collection.mutable.Map

type Dimension = Int
type Target = (Int, Int)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result1 =
      given Context = Context.fromInputPart1(inputLines)
      solver(TreeSet(Path.from(Summit(1, 0))), Summit.target).get.toString

    val result2 =
      given Context = Context.fromInputPart2  (inputLines)
      solver(TreeSet(Path.from(Summit(1, 0))), Summit.target).get.toString


    (s"$result1", s"$result2")

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