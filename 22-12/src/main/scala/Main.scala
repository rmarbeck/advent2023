import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
import scala.annotation.tailrec
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.seqIsParallelizable
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

    val brickYard = BrickYard(lines)

    moveAll(brickYard)

    val nbBricksThatCanBeDesintegrated = brickYard.bricks.map: currentBrick =>
      currentBrick.supportedBricks.map: supportedBrick =>
        brickYard.bricks.filterNot(_ == currentBrick).find(_.supportedBricks.contains(supportedBrick)) match
          case Some(_) => true
          case None => false
      .find(_ == false)
    .filterNot(_.isDefined)
    .length

    val nbBricksThatWouldFall = brickYard.bricks.par.map: currentBrick =>
      val result = removeBricks(List(currentBrick), brickYard)
      result
    .sum

    val (result1, result2) = (s"${nbBricksThatCanBeDesintegrated}", s"${nbBricksThatWouldFall}")

    (s"${result1}", s"${result2}")


case class Coords(x: Int, y: Int, z: Int)


object Coords:
  def from(xs: String, ys: String, zs: String) =
    Coords(xs.toInt, ys.toInt, zs.toInt)

class BrickYard(input: Seq[String]):
  val bricks: List[Brick] = input.map(Brick.fromEncodedString(_)).toList

  override def toString: String = s"has ${bricks.length} elements"

class Brick(val name: String, var start: Coords, var end: Coords):
  var supportedBricks: List[Brick] = List[Brick]()
  def addBrickSupported(brick: Brick) = supportedBricks = brick +: supportedBricks
  def minX = min(this.start.x, this.end.x)
  def maxX = max(this.start.x, this.end.x)
  def minY = min(this.start.y, this.end.y)
  def maxY = max(this.start.y, this.end.y)
  def allXYCoords =
    (maxX - minX, maxY - minY) match
      case (diffX, diffY) if diffX > 0 && diffY == 0 => (0 to diffX).map(increment => Coords(minX+increment, minY, -1))
      case (diffX, diffY) if diffX == 0 && diffY > 0 => (0 to diffY).map(increment => Coords(minX, minY+increment, -1))
      case _ =>List(Coords(minX, minY, -1))

  def canLayOn(other: Brick): Boolean =
    allXYCoords.map(other.allXYCoords.contains(_)).contains(true)

  def moveDownTo(lowestAltitudeTo: Int) =
    end = end.copy(z = lowestAltitudeTo + end.z - start.z)
    start = start.copy(z = lowestAltitudeTo)
  def lowestAltitude: Int = math.min(start.z, end.z)
  def highestAltitude: Int = math.max(start.z, end.z)

  def supportedDirectlyBy(implicit brickYard: BrickYard): List[Brick] =
    brickYard.bricks.filterNot(_ == this).filter(_.supportedBricks.contains(this)).distinct

  def isOnlySupportedByOneOf(bricks: List[Brick])(implicit brickYard: BrickYard): Boolean =
    supportedDirectlyBy.map(bricks.contains(_)).filter(_ == false).length == 0

  override def toString: String = s"$name"

object Brick:
  def fromEncodedString(input: String): Brick =
    input match
      case value @ s"$xs,$ys,$zs~$xe,$ye,$ze" => Brick(value, Coords.from(xs, ys, zs), Coords.from(xe, ye, ze))

def moveAll(brickYard: BrickYard): List[Brick] =
  moveDown(brickYard.bricks.sortBy(_.lowestAltitude), List())

def moveDown(toMoveDownSortedBricks: List[Brick], alreadyInPlaceBricks: List[Brick]): List[Brick] =
  toMoveDownSortedBricks match
    case Nil => alreadyInPlaceBricks
    case head::tail =>
      alreadyInPlaceBricks match
        case Nil =>
          head.moveDownTo(1)
          moveDown(tail, List(head))
        case _ =>
          //calculate lowest altitude it will go and bricks on which it is gonna lay
          val bricksItCanLayOn: List[(Brick, Int)] = alreadyInPlaceBricks.sortBy(_.highestAltitude).reverse.map: currentBrick =>
            (currentBrick, currentBrick.highestAltitude)
          .filter((brick, _) => head.canLayOn(brick))

          val highestAltitude = bricksItCanLayOn.map(_._2).maxOption.getOrElse(0)
          head.moveDownTo(highestAltitude+1)
          val bricksItWillLayOn = bricksItCanLayOn.filter(_._2 == highestAltitude).map(_._1)
          bricksItWillLayOn.foreach(_.addBrickSupported(head))
          moveDown(tail, head +: alreadyInPlaceBricks)

@tailrec
def removeBricks(bricksToRemove: List[Brick], brickYard: BrickYard): Int =
  given BrickYard = brickYard
  val newBricksToRemove = bricksToRemove.flatMap(_.supportedBricks).filterNot(bricksToRemove.contains(_)).filter(_.isOnlySupportedByOneOf(bricksToRemove))
  newBricksToRemove match
    case Nil => bricksToRemove.length - 1
    case value => removeBricks((newBricksToRemove ::: bricksToRemove).distinct, brickYard: BrickYard)


