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

    val resultPart2 = guess(trajectories.toList, 100)

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
  private lazy val coords: List[Long] = List(xInit, yInit, zInit, xSpeed, ySpeed, zSpeed)
  private lazy val xAndY: List[Double] = List(xInit, yInit, xSpeed, ySpeed).map(_.toDouble)

  private def xAndYAtT(t: Double): PointDouble =
    PointDouble(xInit + t * xSpeed, yInit + t * ySpeed, 0d)

  def atT(t: Long): Point = Point(xInit + t*xSpeed, yInit + t*ySpeed, zInit +t*zSpeed)

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

    denomX * denomY *denomZ match
      case 0 =>
        val tests = List((x2 - x1, denomX), (y2 - y1, denomY), (z2 - z1, denomZ)).map((diff, denom) => intersectsOnCoord(diff, denom))
        tests.forall(value => value.left.exists(identity) || value.isRight) && tests.flatMap(_.toOption).distinct.length <= 1
      case _ =>
        val tX = (x2 - x1) / denomX
        tX > 0 && tX == (y2 - y1)/ denomY && tX == (z2 - z1)/ denomZ

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


def guess(trajectories: List[Trajectory], timeMax: Int): Trajectory =
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
  .head

def calculateTrajectoryCrossing(first: Trajectory, second: Trajectory, time1: Long, time2: Long): Trajectory =
  if (time1 > time2)
    calculateTrajectoryCrossing(second, first, time2, time1)
  else
    val (point1, point2) = (first.atT(time1), second.atT(time2))
    val directingVector@ List(mx, my, mz) = point1.coords.zip(point2.coords).map((coord1, coord2) => (coord2 - coord1) / (time2 - time1))
    val List(xInit, yInit, zInit) = point1.coords.zip(directingVector).map((coord1, speed1) => coord1 - (speed1 * time1))

    val result = Trajectory(xInit, yInit, zInit, mx, my, mz)
    println(result)
    result



