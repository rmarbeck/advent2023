import Line.getPlan
import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
import org.apache.commons.math3.linear._

// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching X-12")
  val startTime = Instant.now()

  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")
  println(s"Computing time is ${Duration.between(startTime, Instant.now()).toMillis}ms")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq

    val paths = lines.map(Line.from(_))

    val point1 = Point(19, 13, 0)
    val point2 = Point(18, 19, 0)

    val speed1 = Speed(-2, 1, 0)
    val speed2 = Speed(-1, -1, 0)

    val line1 = Line(point1, speed1)
    val line2 = Line(point2, speed2)

    val test1 = TestZone(Point(200000000000000l,200000000000000l,0), Point(400000000000000l,400000000000000l, 0))



    val toSolve = Array.fill(3, 3)(0d)
    toSolve(0)(0) = 2d
    toSolve(0)(1) = 3d
    toSolve(0)(2) = -2d

    toSolve(1)(0) = -1d
    toSolve(1)(1) = 7d
    toSolve(1)(2) = 6d

    toSolve(2)(0) = 4d
    toSolve(2)(1) = -3d
    toSolve(2)(2) = 5d

    val toSolve2 = Array(Array(2d,3d,-2d), Array(-1d,7d,6d),Array(4d,-3d,5d))

    val matrix = Array2DRowRealMatrix(toSolve2, false)
    val solver = LUDecomposition(matrix).getSolver
    val constants = ArrayRealVector(Array(1d, -2d, 1d), false)
    println(solver.solve(constants))

    val result = paths.zipWithIndex.map: (path1, index) =>
      paths.drop(index+1).map: path2 =>
        path1.intersectionXY(path2).map(test1.contains(_)).getOrElse(false)
    .flatten.filter(_ == true).length

    paths.zipWithIndex.foreach: (path1, index) =>
      paths.drop(index + 1).foreach: path2 =>
        path1.parallel(path2) match
          case Some((value1, value2)) => println(s"$value1 and $value2 => ${value1.getPlanWith(value2)}")
          case _ => ()

    val toSolve3 = paths.zipWithIndex.flatMap: (path1, index) =>
      paths.drop(index + 1).flatMap: path2 =>
        path1.parallel(path2) match
          case Some((value1, value2)) => Some(value1.getPlanWith(value2))
          case _ => None

    toSolve3.length match
      case value if value < 3 => ()
      case _ =>
        toSolve3.foreach: current1 =>
          toSolve3.filter(_ != current1).foreach: current2 =>
            toSolve3.filter(newOne => newOne != current1 && newOne != current2).foreach: current3 =>
              val selections =  current1 :: current2 :: current3 :: Nil

              val solvingArray = selections.map:
                case (x, y, z, _) => Array(x.toDouble, y.toDouble, z.toDouble)
              .toArray

              val constantsArray = selections.map:
                case (_, _, _, constant) => -constant.toDouble
              .toArray

              val matrix2 = Array2DRowRealMatrix(solvingArray, false)
              val solver2 = LUDecomposition(matrix2).getSolver
              val constants2 = ArrayRealVector(constantsArray, false)
              println(s"resultat => ${solver2.solve(constants2)}")

    val (result1, result2) = (s"$result", "")

    (s"${result1}", s"${result2}")


case class Point(x: Long, y: Long, z: Long):
  override def toString: String = s"($x, $y, $z)"

case class PointDouble(x: Double, y: Double, z: Double)

case class TestZone(minPoint: Point, maxPoint: Point):
  def contains(point: PointDouble): Boolean =
    (point.x, point.y) match
      case (x, y) if x >= minPoint.x && x <= maxPoint.x => y match
        case value if value >= minPoint.y && y <= maxPoint.y => true
        case _ => false
      case _ => false


case class Speed(xs: Long, ys: Long, zs: Long)

case class Line(initPoint: Point, speed: Speed):
  def getPlanWith(otherLine: Line): (Long, Long, Long, Long) =
    getPlan(this.initPoint, otherLine.initPoint, Point(this.initPoint.x + this.speed.xs, this.initPoint.y + this.speed.ys, this.initPoint.z + this.speed.zs))
  private def isParallel(otherLine: Line): Boolean =
    val List(x1, y1, xs1, ys1, x2, y2, xs2, ys2) = List(this.initPoint.x, this.initPoint.y, this.speed.xs, this.speed.ys, otherLine.initPoint.x, otherLine.initPoint.y, otherLine.speed.xs, otherLine.speed.ys).map(_.toDouble)
    val denom1 = (xs1 / xs2 - ys1 / ys2)
    val denom2 = (xs2 / xs1 - ys2 / ys1)

    val allDenoms = ys1 * ys2 * xs1 * xs2 * denom1 * denom2
    allDenoms == 0
  def parallel(otherLine: Line): Option[(Line, Line)] =
    isParallel(otherLine) match
      case true => Some((this, otherLine))
      case _ => None

  def isTheSame(otherLine: Line): Boolean =
    val t = (this.initPoint.x - otherLine.initPoint.x) / otherLine.speed.xs
    println(s" ============+> ${otherLine.initPoint.x + otherLine.speed.xs * t} ${this.initPoint.x} <+==========")
    println(s" ============+> ${otherLine.initPoint.y + otherLine.speed.ys *t} ${this.initPoint.y} <+==========")
    (otherLine.initPoint.y + otherLine.speed.ys *t) == this.initPoint.y && (otherLine.initPoint.z + otherLine.speed.zs *t) == this.initPoint.z

  def intersectionXY(otherLine: Line): Option[PointDouble] =
    isParallel(otherLine) match
      case true => loggerAOC.debug(s"impossible : $this <-> $otherLine : ${isTheSame(otherLine)}"); None
      case _ =>
        val List(x1, y1, xs1, ys1, x2, y2, xs2, ys2) = List(this.initPoint.x, this.initPoint.y, this.speed.xs, this.speed.ys, otherLine.initPoint.x, otherLine.initPoint.y, otherLine.speed.xs, otherLine.speed.ys).map(_.toDouble)
        val denom1 = (xs1 / xs2 - ys1 / ys2)
        val denom2 = (xs2 / xs1 - ys2 / ys1)

        val t2 = ((y2 - y1) / ys1 - (x2 - x1) / xs1) / denom2
        val t1 = ((y1 - y2) / ys2 - (x1 - x2) / xs2) / denom1
        loggerAOC.trace(s"$t1 - $t2")
        (t1, t2) match
          case (val1, val2) if val1 >= 0 && val2 >= 0 => Some(this.atT(t1))
          case _ => None

  override def toString: String = s"$initPoint"

  def atT(t: Double): PointDouble =
    loggerAOC.trace(s"(${this.initPoint.x + t*this.speed.xs}, ${this.initPoint.y + t*this.speed.ys})")
    PointDouble(this.initPoint.x + t*this.speed.xs, this.initPoint.y + t*this.speed.ys, 0d)

object Line:
  def getPlan(point1: Point, point2: Point, point3: Point): (Long, Long, Long, Long) =
    val xFactor = ((point2.y - point1.y) * (point3.z - point1.z)) - ((point2.z - point1.z) * (point3.y - point1.y))
    val yFactor = - ((point2.x - point1.x) * (point3.z - point1.z)) - ((point2.z - point1.z) * (point3.x - point1.x))
    val zFactor = ((point2.x - point1.x) * (point3.y - point1.y)) - ((point2.y - point1.y) * (point3.x - point1.x))
    val c = -(xFactor*point1.x + yFactor*point1.y + zFactor*point1.z)
    (xFactor, yFactor, zFactor, c)
  def from(input: String) =
    input match
      case s"$x, $y, $z @ $xs, $ys, $zs" => Line(Point(x.trim.toLong, y.trim.toLong, z.trim.toLong), Speed(xs.trim.toLong, ys.trim.toLong, zs.trim.toLong))