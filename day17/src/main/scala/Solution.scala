import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.TreeSet
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

val maxStepsInOneDirectionPart1 = 3
val minStepsInOneDirectionPart1 = 1
val maxStepsInOneDirectionPart2 = 10
val minStepsInOneDirectionPart2 = 4

type HeatMapper = Position => Long
type Dimension = (Int, Int)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val (height: Int, width: Int) = (inputLines.length, inputLines.head.length)

    val heats = inputLines.map(_.toCharArray.map(_.asDigit)).toArray

    given Dimension = (width, height)

    given HeatMapper =
      (for
        y <- heats.indices; x <- heats.head.indices
      yield
        Position(x, y) -> heats(y)(x).toLong
        ).toMap.apply


    import scala.collection.parallel._
    import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
    val List(result1, result2) =
      Seq(CrucibleConstraints.Part1, CrucibleConstraints.Part2).par.map:
        constraints =>
          given CrucibleConstraints = constraints
          heatLoss(TreeSet(startingCrucible: _*), Set(), Position(width - 1, height - 1))
      .toList

    (s"$result1", s"$result2")

end Solution

def startingCrucible(using CrucibleConstraints, Dimension): Seq[(Long, Crucible)] =
  summon[CrucibleConstraints] match
    case CrucibleConstraints.Part1 => Seq((0L, Crucible.init(Left2Right)))
    case _ => Seq((0L, Crucible.init(Left2Right)), (0L, Crucible.init(Up2Down)))

case class CrucibleConstraints(maxStepsInOneDirection: Int, minStepsInOneDirection: Int)

object CrucibleConstraints:
  val Part1: CrucibleConstraints = CrucibleConstraints(maxStepsInOneDirectionPart1, minStepsInOneDirectionPart1)
  val Part2: CrucibleConstraints = CrucibleConstraints(maxStepsInOneDirectionPart2, minStepsInOneDirectionPart2)

case class Position(x: Int, y: Int)(using dimension: Dimension):
  private inline def exists: Option[Position] = Option.when(x >= 0 && x < dimension._1 && y >=0 && y < dimension._2)(this)
  @targetName("add")
  def +(dir: Direction): Option[Position] =
    (dir match
      case Left2Right => this.copy(x = x + 1)
      case Right2Left => this.copy(x = x - 1)
      case Up2Down => this.copy(y = y + 1)
      case Down2Up => this.copy(y = y - 1)
      ).exists

case class Crucible(position: Position, lastDir: Direction, lastDirCounter: Int)(using dimension: Dimension, crucibleConstraints: CrucibleConstraints):
  private def nextStep(newDir: Direction): Option[Crucible] =
    def dirCounter: Int = if (newDir == lastDir) then lastDirCounter + 1 else 1
    (position + newDir).map(newPos => Crucible(newPos, newDir, dirCounter))

  lazy val turn90CW: Option[Crucible] = nextStep(lastDir.turnCW)
  lazy val turn90CCW: Option[Crucible] = nextStep(lastDir.turnCCW)
  lazy val forward: Option[Crucible] = nextStep(lastDir)

  lazy val andMore: Set[Crucible] =
    if (lastDirCounter >= crucibleConstraints.minStepsInOneDirection)
      (lastDirCounter to crucibleConstraints.maxStepsInOneDirection).map(value => this.copy(lastDirCounter = value)).toSet
    else
      Set(this)

  def next: Seq[Crucible] =
    lastDirCounter match
      case value if value < crucibleConstraints.minStepsInOneDirection => Seq(forward).flatten
      case value if value < crucibleConstraints.maxStepsInOneDirection => Seq(turn90CCW, turn90CW, forward).flatten
      case _ => Seq(turn90CCW, turn90CW).flatten

object Crucible:
  given ordering: Ordering[Crucible] = Ordering.by(nav => (-nav.position.x, -nav.position.y, nav.lastDirCounter, nav.lastDir.ordinal))
  def init(direction: Direction)(using CrucibleConstraints, Dimension): Crucible = Crucible(Position(0, 0), direction, 0)

@tailrec
def heatLoss(toExplore: TreeSet[(Long, Crucible)], forbidden: Set[Crucible], goal: Position)(using dimension: Dimension, heatMapper: HeatMapper, crucibleConstraints: CrucibleConstraints): Long =
  toExplore match
    case empty if empty.isEmpty => 0
    case notEmpty =>
      val (head, tail) = (toExplore.head, toExplore.tail)
      if (forbidden.contains(head._2))
        heatLoss(tail, forbidden, goal)
      else
        val currentHeatLoss = head._1
        if (head._2.position == goal && head._2.lastDirCounter >= crucibleConstraints.minStepsInOneDirection)
          currentHeatLoss
        else
          heatLoss(tail ++ head._2.next.map(newPosition => (currentHeatLoss + heatMapper.apply(newPosition.position), newPosition)), forbidden ++ head._2.andMore, goal)

enum Direction:
  case Left2Right, Up2Down, Right2Left, Down2Up

  def turnCW: Direction = Direction.fromOrdinal((ordinal + 1) % Direction.values.length)
  def turnCCW: Direction = Direction.fromOrdinal((ordinal + 3) % Direction.values.length)

export Direction.*
