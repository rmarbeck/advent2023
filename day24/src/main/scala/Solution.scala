import scala.annotation.tailrec
import collection.parallel.*
import collection.parallel.CollectionConverters.seqIsParallelizable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val trajectories = inputLines.map(Trajectory.from)

    val (min, max) = trajectories.length match
      case 5 => (7l, 27l)
      case _ => (200_000_000_000_000l, 400_000_000_000_000l)

    val testZone = TestZone(Point(min, min, 0), Point(max, max, 0))

    val resultPart1 = (for
      case (first, index) <- trajectories.zipWithIndex
      second <- trajectories.drop(index)
      if first.intersectionXY(second).exists(testZone.contains)
    yield
      1
    ).sum

    val resultPart2 = calculateBySpeed(trajectories.toList).coords.take(3).sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Point(x: Long, y: Long, z: Long):
  def coords = List(x, y, z)
case class PointDouble(x: Double, y: Double, z: Double)

case class TestZone(minPoint: Point, maxPoint: Point):
  def contains(point: PointDouble): Boolean =
    (point.x, point.y) match
      case (x, y) if x >= minPoint.x && x <= maxPoint.x => y match
        case value if value >= minPoint.y && y <= maxPoint.y => true
        case _ => false
      case _ => false

case class Trajectory(xInit: Long, yInit: Long, zInit: Long, xSpeed: Long, ySpeed: Long, zSpeed: Long):
  lazy val isValid: Boolean = xInit >= 0 && yInit >= 0 && zInit >= 0
  lazy val coords: List[Long] = List(xInit, yInit, zInit, xSpeed, ySpeed, zSpeed)
  private lazy val xAndY: List[Double] = List(xInit, yInit, xSpeed, ySpeed).map(_.toDouble)

  private def xAndYAtT(t: Double): PointDouble =
    PointDouble(xInit + t * xSpeed, yInit + t * ySpeed, 0d)

  import collection.mutable.{Map, HashMap}
  private val cachedAtT: Map[Long, Point] = HashMap[Long, Point]()
  def atT(t: Long): Point =
    //cachedAtT.getOrElseUpdate(t, Point(xInit + t*xSpeed, yInit + t*ySpeed, zInit +t*zSpeed))
    Point(xInit + t * xSpeed, yInit + t * ySpeed, zInit + t * zSpeed)

  def intersects(otherTrajectory: Trajectory): Boolean =
    def intersectsOnCoord(diff: Long, denom: Long): Either[Boolean, Long] =
      denom match
        case 0 => Left(diff == 0)
        case value => Right(diff/value)

    val List(x1, y1, z1, xs1, ys1, zs1) = coords
    val List(x2, y2, z2, xs2, ys2, zs2) = otherTrajectory.coords

    val denomX = (xs2 - xs1)
    val denomY = (ys2 - ys1)
    val denomZ = (zs2 - zs1)

    denomX * denomY * denomZ match
      case 0 =>
        val tests = List((x2 - x1, denomX), (y2 - y1, denomY), (z2 - z1, denomZ)).map((diff, denom) => intersectsOnCoord(diff, denom))
        tests.forall(value => value.left.exists(identity) || value.isRight) && tests.flatMap(_.toOption).distinct.length <= 1 && tests.flatMap(_.toOption).forall(_ < 0)
      case _ =>
        val tX = - (x2 - x1) / denomX
        tX > 0 && tX == - (y2 - y1)/ denomY && tX == - (z2 - z1)/ denomZ

  def intersectionXY(otherTrajectory: Trajectory): Option[PointDouble] =
    val List(x1, y1, xs1, ys1) = xAndY
    val List(x2, y2, xs2, ys2) = otherTrajectory.xAndY
    val denom1 = (xs1 / xs2 - ys1 / ys2)
    val denom2 = (xs2 / xs1 - ys2 / ys1)

    ys1 * ys2 * xs1 * xs2 * denom1 * denom2 match
      case 0 => None // Lines are parallels
      case _ =>
        val t2 = ((y2 - y1) / ys1 - (x2 - x1) / xs1) / denom2
        val t1 = ((y1 - y2) / ys2 - (x1 - x2) / xs2) / denom1
        (t1, t2) match
          case (val1, val2) if val1 >= 0 && val2 >= 0 => Some(this.xAndYAtT(t1))
          case _ => None

object Trajectory:
  def from(rawString: String): Trajectory =
    val List(xInit, yInit, zInit, xSpeed, ySpeed, zSpeed) = rawString match
      case s"$xi, $yi, $zi @ $xs, $ys, $zs" => List(xi, yi, zi, xs, ys, zs).map(_.trim).map(_.toLong)
      case _ => throw Exception("Not supported")
    Trajectory(xInit, yInit, zInit, xSpeed, ySpeed, zSpeed)

case class XTrajectory(xInit: Long, xSpeed: Long, parent: Trajectory):
  lazy val isValid: Boolean = xInit >= 0
  lazy val coords: List[Long] = List(xInit, xSpeed)
  import collection.mutable.{Map, HashMap}
  private val cachedAtT: Map[Long, Long] = HashMap[Long, Long]()
  def atT(t: Long): Long =
    cachedAtT.getOrElseUpdate(t, xInit + t * xSpeed)

  def intersects(otherXTrajectory: XTrajectory): Boolean =
    val List(x1, xs1) = coords
    val List(x2, xs2) = otherXTrajectory.coords

    val denomX = (xs2 - xs1)

    denomX match
      case 0 =>
        x2 - x1 == 0
      case _ =>
        val tX = -(x2 - x1) / denomX
        tX > 0


@tailrec
def guessOnX(xTrajectories: List[XTrajectory], combos: List[List[XTrajectory]], time1: Int = 1, time2: Int = 2, timeMax: Int, trajectories: List[Trajectory]): Option[XTrajectory] =
  def checkOnTrajectories(first: XTrajectory, second: XTrajectory): Boolean =
    val target = calculateTrajectoryCrossing(first.parent, second.parent, time1, time2)
    target.isValid && trajectories.forall(_ intersects target)

  time1 == timeMax match
    case true => None
    case false =>
      val result = combos.find:
        case List(first, second) =>
          val target = calculateXTrajectoryCrossing(first, second, time1, time2)
          target.isValid && xTrajectories.forall(_ intersects target) && checkOnTrajectories(first, second)
        case _ => throw Exception("Not supported")
      result match
        case Some(value) =>
          val List(first, second) = value
          Some(calculateXTrajectoryCrossing(first, second, time1, time2))
        case None =>
          time2 == timeMax match
            case true => guessOnX(xTrajectories, combos, time1 + 1, time1 + 2, timeMax, trajectories)
            case false => guessOnX(xTrajectories, combos, time1, time2 + 1, timeMax, trajectories)

def guessOnX(trajectories: List[Trajectory], start: Int, timeMax: Int): Option[XTrajectory] =
  val xTrajectories = trajectories.map(current => XTrajectory(current.xInit, current.xSpeed, current))
  val combos = xTrajectories.combinations(2).toList
  val combosAndReverse = combos.head :: combos.head.reverse :: Nil
  guessOnX(xTrajectories, combosAndReverse, start, start + 1, timeMax, trajectories)

@tailrec
def guess3Rec(trajectories: List[Trajectory], combos: List[List[Trajectory]], time1: Int = 1, time2: Int = 2, timeMax: Int): Option[Trajectory] =
  time1 == timeMax match
    case true => None
    case false =>
      val result = combos.par.find:
        case List(first, second) =>
          val target = calculateTrajectoryCrossing(first, second, time1, time2)
          target.isValid && trajectories.forall(_ intersects target)
        case _ => throw Exception("Not supported")
      result match
        case Some(value) =>
          val List(first, second) = value
          Some(calculateTrajectoryCrossing(first, second, time1, time2))
        case None =>
          time2 == timeMax match
            case true => guess3Rec(trajectories, combos, time1 + 1, time1 + 2, timeMax)
            case false => guess3Rec(trajectories, combos, time1, time2 + 1, timeMax)

def guess3(trajectories: List[Trajectory], timeMax: Int): Trajectory =
  val combos = trajectories.combinations(2).toList
  val combosAndReverse = combos.head :: combos.head.reverse :: Nil
  guess3Rec(trajectories, combosAndReverse, 1, 2, timeMax).get


def guess(trajectories: List[Trajectory], timeMax: Int): Trajectory =
  val combos = trajectories.combinations(2).toList
  val combosAndReverse = combos ::: combos.map(_.reverse)
  (1 to timeMax).flatMap:
    time1 => (1+time1 to timeMax).flatMap:
      time2 =>
        val result = combosAndReverse.find:
          case List(first, second) =>
            val target = calculateTrajectoryCrossing(first, second, time1, time2)
            trajectories.filter(current => current != first && current != second).forall(_ intersects target)
          case _ => throw Exception("Not possible")
        result.map:
          case List(first, second) =>
            val target = calculateTrajectoryCrossing(first, second, time1, time2)
            println(s"[$target] $first and $second, $time1 - $time2 ${trajectories.map(_ intersects target)}")
            calculateTrajectoryCrossing(first, second, time1, time2)
          case _ => throw Exception("Not supported")
  .headOption.get


def guess2(trajectories: List[Trajectory], timeMax: Int): Trajectory =
  val combos = trajectories.combinations(2).toList
  val combosAndReverse = combos :: combos.map(_.reverse)
  combos.flatMap:
    case List(first, second) =>
      for
        time1 <- 1 to timeMax
        time2 <- time1+1 to timeMax
        target = calculateTrajectoryCrossing(first, second, time1, time2)
        if trajectories.filter(current => current != first && current != second).forall(_ intersects target)
      yield
        target
    case _ => throw Exception("Not possible")
  .find(_ => true).get

def calculateTrajectoryCrossing(first: Trajectory, second: Trajectory, time1: Long, time2: Long): Trajectory =
  if (time1 > time2)
    calculateTrajectoryCrossing(second, first, time2, time1)
  else
    val (point1, point2) = (first.atT(time1), second.atT(time2))
    val directingVector@ List(mx, my, mz) = point1.coords.zip(point2.coords).map((coord1, coord2) => (coord2 - coord1) / (time2 - time1))
    val List(xInit, yInit, zInit) = point1.coords.zip(directingVector).map((coord1, speed1) => coord1 - (speed1 * time1))

    val result = Trajectory(xInit, yInit, zInit, mx, my, mz)
    result

def calculateXTrajectoryCrossing(first: XTrajectory, second: XTrajectory, time1: Long, time2: Long): XTrajectory =
  if (time1 > time2)
    calculateXTrajectoryCrossing(second, first, time2, time1)
  else
    val (point1, point2) = (first.atT(time1), second.atT(time2))
    val mx = (point2 - point1) / (time2 - time1)
    val xInit = point1 - (mx  * time1)

    XTrajectory(xInit, mx, first.parent)

def calculateBySpeed(trajectories: List[Trajectory], speedMin: Long = 0, speedMax: Long = 0): Trajectory =
  def forSpeed(first: Trajectory, second: Trajectory, xSpeed: Long, ySpeed: Long): Either[Boolean, Trajectory] =
    val List(x1, y1, z1, xs1, ys1, zs1) = first.coords.map(_.toDouble)
    val List(x2, y2, z2, xs2, ys2, zs2) = second.coords.map(_.toDouble)

    val num = (x2 - x1) / (xSpeed - xs1) + (y1 - y2) / (ySpeed - ys1)
    val denom = ((xSpeed - xs2) / (xSpeed - xs1)) - ((ySpeed - ys2) / (ySpeed - ys1))

    denom match
      case 0 => Left(false)
      case _ =>
        val t2 = num / denom
        val t1 = (t2 * (xSpeed - xs2) + x1 - x2) / (xSpeed - xs1)

        t1 > 0 && t2 >0 && t1.toLong != t2.toLong match
          case true =>
            val target = calculateTrajectoryCrossing(first, second, t1.toLong, t2.toLong)
            if (target.isValid && trajectories.filter(current => current != first && current != second).forall(_ intersects target))
              Right(target)
            else
              Left(true)
          case false => Left(true)

  def speedsGenerator = Iterator.iterate(Spiral.Start)(_.next).map(value => (value.x, value.y))

  val result = speedsGenerator.map:
    (xSpeed, ySpeed) =>
      val possibleTrajectoriesToAnalyse = trajectories.filter(current => current.xSpeed != xSpeed && current.ySpeed != ySpeed)
      possibleTrajectoriesToAnalyse.sliding(2, 1).map:
        case List(first, second) =>
          forSpeed(first, second, xSpeed, ySpeed)
      .find:
        case Left(true) | Right(_) => true
        case _ => false
      .flatMap:
        case Left(_) => None
        case Right(value) => Some(value)
  .find(_.isDefined).get

  result.get


final case class Spiral(
                         x: Long, y: Long,
                         dx: Long, dy: Long,
                         count: Long, limit: Long,
                       ):
  def next: Spiral =
    if count > 0 then
      copy(x = x + dx, y = y + dy, count = count - 1)
    else if dy == 0 then
      copy(x = x + dx, y = y + dy, dy = dx, dx = -dy, count = limit)
    else
      copy(x = x + dx, y = y + dy, dy = dx, dx = -dy,
        count = limit + 1, limit = limit + 1)
  end next
end Spiral

object Spiral:
  final val Start = Spiral(0, 0, 1, 0, 0, 0)

