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


def calculateTrajectoryCrossing(first: Trajectory, second: Trajectory, time1: Long, time2: Long): Trajectory =
  if (time1 > time2)
    calculateTrajectoryCrossing(second, first, time2, time1)
  else
    val (point1, point2) = (first.atT(time1), second.atT(time2))
    val directingVector@ List(mx, my, mz) = point1.coords.zip(point2.coords).map((coord1, coord2) => (coord2 - coord1) / (time2 - time1))
    val List(xInit, yInit, zInit) = point1.coords.zip(directingVector).map((coord1, speed1) => coord1 - (speed1 * time1))

    val result = Trajectory(xInit, yInit, zInit, mx, my, mz)
    result

def calculateBySpeed(trajectories: List[Trajectory], speedMin: Long = 0, speedMax: Long = 0): Trajectory =
  def forSpeed(first: Trajectory, second: Trajectory, xSpeed: Long, ySpeed: Long): Either[String, Option[Trajectory]] =
    val List(x1, y1, z1, xs1, ys1, zs1) = first.coords.map(_.toDouble)
    val List(x2, y2, z2, xs2, ys2, zs2) = second.coords.map(_.toDouble)

    val num = (x2 - x1) / (xSpeed - xs1) + (y1 - y2) / (ySpeed - ys1)
    val denom = ((xSpeed - xs2) / (xSpeed - xs1)) - ((ySpeed - ys2) / (ySpeed - ys1))

    denom match
      case 0 => Left("Denom is null")
      case _ =>
        val t2 = num / denom
        val t1 = (t2 * (xSpeed - xs2) + x1 - x2) / (xSpeed - xs1)

        t1 > 0 && t2 >0 && t1.toLong != t2.toLong match
          case true =>
            val target = calculateTrajectoryCrossing(first, second, t1.toLong, t2.toLong)
            if (target.isValid && trajectories.filter(current => current != first && current != second).forall(_ intersects target))
              Right(Some(target))
            else
              Right(None)
          case false => Right(None)

  def speedsGenerator = Iterator.iterate(Spiral.Start)(_.next).map(value => (value.x, value.y))

  val result = speedsGenerator.flatMap:
    (xSpeed, ySpeed) =>
      val possibleTrajectoriesToAnalyse = trajectories.filter(current => current.xSpeed != xSpeed && current.ySpeed != ySpeed)
      possibleTrajectoriesToAnalyse.sliding(2, 1).map:
        case List(first, second) =>
          forSpeed(first, second, xSpeed, ySpeed)
        case _ => throw Exception("Not Possible")
      .find:
        case Right(_) => true
        case _ => false
      .flatMap:
        case Left(_) => None
        case Right(value) => value
  .next

  result

def speeds(x: Long, y: Long, counter: Int = 0): LazyList[(Long, Long)] =
  val next = counter match
    case 0 => ((-x)+1, y)
    case 1 => (x, (-y)+1)
    case 2 => (-x, y)
    case 3 => (x, -y)
  (x, y) #:: speeds(next._1, next._2, (counter + 1)%4)

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

