import scala.annotation.tailrec
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.seqIsParallelizable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val flyingBricks = inputLines.map(FlyingBrick.from)

    val brickContainer = BrickContainer.fromUnSortedFlyingBricks(flyingBricks)

    val (canBeDisintegrated, cannotBeDisintegrated) = brickContainer.staticBricks.par.partition:
        _.listSupported match
          case Nil => true
          case supportedList => supportedList.forall(_.listSupporting.length > 1)

    val resultPart2 = cannotBeDisintegrated.par.map(_.computeHowManyFalls).sum

    val result1 = s"${canBeDisintegrated.length}"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Coordinates(x: Int, y: Int, z: Int)

case class Cube(coordinates: Coordinates):
  def goDown(numberOfSteps: Int): Cube = Cube(coordinates.copy(z = coordinates.z - numberOfSteps))
  def isRightBelowOneOf(others: List[Cube]): Boolean =
    others.exists:
      cube => cube.coordinates.x == this.coordinates.x && cube.coordinates.y == this.coordinates.y && cube.coordinates.z == this.coordinates.z + 1

case class FlyingBrick(name: String, cubes: List[Cube]):
  def goDown(numberOfSteps: Int): FlyingBrick = this.copy(cubes = cubes.map(_.goDown(numberOfSteps)))

  lazy val lowestCube = cubes.map(_.coordinates.z).min

  lazy val highsAndLows: Map[(Int, Int), (Int, Int)] =
    val altitudes = cubes.groupMap(current => (current.coordinates.x, current.coordinates.y))(_.coordinates.z)
    altitudes.map:
      case ((x, y), zList) => (x, y) -> (zList.max, zList.min)

  lazy val lows: List[(Int, Int, Int)] = highsAndLows.toList.map:
    case ((x, y), (_, minimum)) => (x, y, minimum)

  override def toString: String = s"Brick : $name [$lowestCube]"

object FlyingBrick:
  def from(inputString: String): FlyingBrick =
    val cubes = inputString match
      case s"$xs,$ys,$zs~$xe,$ye,$ze" =>
        val (starts, ends) = List(xs, ys, zs, xe, ye, ze).map(_.toInt).splitAt(3)
        val couples = starts.zip(ends)
        val List(xMin, yMin, zMin): List[Int] = couples.map(math.min(_, _))
        val List(xMax, yMax, zMax): List[Int] = couples.map(math.max(_, _))
        for
          x <- xMin to xMax
          y <- yMin to yMax
          z <- zMin to zMax
        yield
          Cube(Coordinates(x, y, z))
      case _ => throw Exception("Not supported")
    FlyingBrick(inputString, cubes.toList)

case class StaticBrick(flyingBrick: FlyingBrick, brickContainer: BrickContainer):
  lazy val highs: Map[(Int, Int), Int] = flyingBrick.highsAndLows.map:
    case ((x, y), (maximum, _)) => (x, y) -> maximum

  private def support(other: StaticBrick): Boolean = flyingBrick.cubes.exists(_.isRightBelowOneOf(other.flyingBrick.cubes))
  lazy val listSupported: List[StaticBrick] =
    brickContainer.staticBricks.filterNot(_ == this).filter(support)

  lazy val listSupporting: List[StaticBrick] =
    def isSupportedBy(other: StaticBrick): Boolean = other.support(this)
    brickContainer.staticBricks.filterNot(_ == this).filter(isSupportedBy)

  lazy val computeHowManyFalls: Int =
    @tailrec
    def compute(currentBricks: List[StaticBrick], toRemove: List[StaticBrick] = Nil, currentCount: Int = 0): Int =
      currentBricks match
        case Nil => currentCount
        case head :: tail =>
          val falling = head.listSupported.filter:
            _.listSupporting.filterNot(toRemove.contains).length <= 1
          compute(tail ::: falling, head +: toRemove, currentCount + falling.length)

    compute(List(this))

case class BrickContainer(sortedFlyingBricks: List[FlyingBrick]):
  val staticBricks: List[StaticBrick] =
    sortedFlyingBricks.foldLeft(BuildingContainer()):
      (acc, newFlyingBrick) => acc.add(newFlyingBrick, this)
  .getBricks

  override def toString: String = s"${staticBricks.map(_.flyingBrick).foreach(println)}"

object BrickContainer:
  def fromUnSortedFlyingBricks(unSortedFlyingBricks: Seq[FlyingBrick]): BrickContainer =
    BrickContainer(unSortedFlyingBricks.sortBy(_.lowestCube).toList)

case class BuildingContainer(staticBricks: List[StaticBrick] = Nil, highestLevels: Map[(Int, Int), Int] = Map()):
  def getBricks = staticBricks
  def add(flyingBrick: FlyingBrick, brickContainer: BrickContainer): BuildingContainer =
    def newHighestLevelsWith(staticBrick: StaticBrick): Map[(Int, Int), Int] =
      val currentBricksHighs = staticBrick.highs
      (highestLevels.keySet ++ currentBricksHighs.keySet).map:
        (x, y) => (x, y) -> math.max(highestLevels.getOrElse((x, y), 1), currentBricksHighs.getOrElse((x, y), 0) + 1)
      .toMap
    def dropBrick: StaticBrick =
      val toDrop = flyingBrick.lows.map:
        (x, y, z) => highestLevels.get((x, y)) match
          case Some(currentValue) => z - currentValue
          case None => z - 1
      .min
      StaticBrick(flyingBrick.goDown(toDrop), brickContainer)
    val newStaticBrick = dropBrick
    val newHighLevels = newHighestLevelsWith(newStaticBrick)
    BuildingContainer(newStaticBrick +: staticBricks, newHighLevels)