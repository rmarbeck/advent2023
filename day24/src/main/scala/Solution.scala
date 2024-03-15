object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val trajectories = inputLines.map(Trajectory.from)

    val (min, max) = (200_000_000_000_000l, 400_000_000_000_000l)

    val testZone = TestZone(Point(min, min, 0), Point(max, max, 0))

    val result = (for
      case (first, index) <- trajectories.zipWithIndex
      second <- trajectories.drop(index)
      if first.intersectionXY(second).map(testZone.contains).getOrElse(false)
    yield
      1
    ).sum

    val result1 = s"$result"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

case class Point(x: Long, y: Long, z: Long)
case class PointDouble(x: Double, y: Double, z: Double)

case class TestZone(minPoint: Point, maxPoint: Point):
  def contains(point: PointDouble): Boolean =
    (point.x, point.y) match
      case (x, y) if x >= minPoint.x && x <= maxPoint.x => y match
        case value if value >= minPoint.y && y <= maxPoint.y => true
        case _ => false
      case _ => false

case class Trajectory(xInit: Long, yInit: Long, zInit: Long, xSpeed: Long, ySpeed: Long, zSpeed: Long):
  private lazy val xAndY: List[Double] = List(xInit, yInit, xSpeed, ySpeed).map(_.toDouble)

  def atT(t: Double): PointDouble =
    PointDouble(xInit + t * xSpeed, yInit + t * ySpeed, 0d)

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
          case (val1, val2) if val1 >= 0 && val2 >= 0 => Some(this.atT(t1))
          case _ => None

object Trajectory:
  def from(rawString: String): Trajectory =
    val List(xInit, yInit, zInit, xSpeed, ySpeed, zSpeed) = rawString match
      case s"$xi, $yi, $zi @ $xs, $ys, $zs" => List(xi, yi, zi, xs, ys, zs).map(_.trim).map(_.toLong)
      case _ => throw Exception("Not supported")
    Trajectory(xInit, yInit, zInit, xSpeed, ySpeed, zSpeed)

