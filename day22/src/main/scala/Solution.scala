import scala.annotation.tailrec
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.seqIsParallelizable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val bricks = inputLines.map(Brick.from)

    val container = Container()
    bricks.sortBy(_.lowestCube).foreach(container.dropBrick)

    //container.bricks.foreach(println)
    println("Ready")

    val resultPart1 = container.bricks.par.count:
      current =>
        val others = container.bricks.filterNot(_ == current)
        val bricksSupported = current.listSupported(others)
        val canBeDisintegrated = bricksSupported match
          case Nil => true
          case supportedList =>
            supportedList.forall:
              currentSupported =>
                val othersWithoutCurrent = others.filterNot(_ == currentSupported)
                currentSupported.countSupporting(othersWithoutCurrent) >= 1

        canBeDisintegrated

    val result1 = s"$resultPart1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

case class Coordinates(x: Int, y: Int, z: Int):
  def asList = List(x, y , z)
  def asTuple = (x, y , z)

case class Cube(coordinates: Coordinates):
  def goDown(numberOfSteps: Int): Cube = Cube(coordinates.copy(z = coordinates.z - numberOfSteps))
  def isRightBelowOneOf(others: List[Cube]): Boolean =
    others.exists:
      cube => cube.coordinates.x == this.coordinates.x && cube.coordinates.y == this.coordinates.y && cube.coordinates.z == this.coordinates.z + 1

case class Brick(name: String, cubes: List[Cube]):
  def overlap(other: Brick): Boolean = (other.cubes.map(_.coordinates) intersect cubes.map(_.coordinates)).nonEmpty
  def support(other: Brick): Boolean = cubes.exists(_.isRightBelowOneOf(other.cubes))
  def listSupported(others: List[Brick]): List[Brick] = others.filter(support)
  def countSupported(others: List[Brick]): Int = others.count(support)
  def isSupportedBy(other: Brick): Boolean = other.support(this)
  def countSupporting(others: List[Brick]): Int = others.count(isSupportedBy)
  def listSupporting(others: List[Brick]): List[Brick] = others.filter(isSupportedBy)
  def goDown(numberOfSteps: Int): Brick = this.copy(cubes = cubes.map(_.goDown(numberOfSteps)))

  lazy val lowestCube = cubes.map(_.coordinates.z).min

  lazy val highsAndLows: Map[(Int, Int), (Int, Int)] =
    val altitudes = cubes.groupMap(current => (current.coordinates.x, current.coordinates.y))(_.coordinates.z)
    altitudes.map(current => current._1 -> (current._2.max, current._2.min))

  lazy val highs: List[(Int, Int, Int)] = highsAndLows.map(current => (current._1._1, current._1._2, current._2._1)).toList
  lazy val lows: List[(Int, Int, Int)] = highsAndLows.map(current => (current._1._1, current._1._2, current._2._2)).toList

  override def toString: String = s"Brick : $name [$lowestCube]"

object Brick:
  def from(inputString: String): Brick =
    val cubes = inputString match
      case s"$xs,$ys,$zs~$xe,$ye,$ze" =>
        val List(xStart, yStart, zStart, xEnd, yEnd, zEnd) = List(xs, ys, zs, xe, ye, ze).map(_.toInt)
        val couples = List((xStart, xEnd), (yStart, yEnd), (zStart, zEnd))
        val List(xRange, yRange, zRange): List[Int] = couples.map(couple => (couple._1 - couple._2).abs)
        val List(xMin, yMin, zMin): List[Int] = couples.map(couple => math.min(couple._1, couple._2))
        for
          x <- xMin to xMin + xRange
          y <- yMin to yMin + yRange
          z <- zMin to zMin + zRange
        yield
          Cube(Coordinates(x, y, z))
      case _ => throw Exception("Not supported")
    Brick(inputString, cubes.toList)

class Container(var bricks: List[Brick] = List()):
  import scala.collection.mutable.Map
  val highestLevels: Map[(Int, Int), Int] = Map()
  def addBrickInFinalPosition(brick: Brick): Unit =
    brick.highs.foreach:
      (x, y, z) => highestLevels.get((x, y)) match
        case Some(currentValue) if currentValue < z => highestLevels.update((x, y), z)
        case None => highestLevels.update((x, y), z)
        case _ => ()
    bricks = brick +: bricks

  def brickIsNotTouchingAny(brick: Brick): Boolean =
    brick.lows.forall:
      (x, y, z) => highestLevels.get((x, y)) match
        case Some(currentValue) if currentValue < z => true
        case None => true
        case _ => false

  @tailrec
  final def dropBrick(brick: Brick): Unit =
    val movedDown = brick.goDown(1)
    movedDown.lowestCube > 1 && brickIsNotTouchingAny(movedDown) match
      case true => dropBrick(movedDown)
      case false => addBrickInFinalPosition(brick)