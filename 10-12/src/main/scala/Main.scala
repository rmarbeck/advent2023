import com.typesafe.scalalogging.Logger

import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
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

    val maze = lines.map(_.toCharArray).toArray

    println(s"maze size : ${maze(0).length} and ${maze.length}")

    val pipes = Seq("|", "-", "7", "F", "L", "J", "S", ".")

    val (result1, result2) = (s"${resolveMaze(maze)}", "")

    (s"${result1}", s"${result2}")

enum Direction:
  case North, East, South, West

def resolveMaze(maze: Array[Array[Char]]): Int =
  def findStart: (Int, Int) =
    var i, j = 0
    var found = false
    while (found == false) {
      if (maze(i)(j) == 'S') {
        found = true
      } else {
        if (j == maze(0).length-1) {
          j = 0
          i = i + 1
        } else {
          j = j +1
        }
      }
    }

    println(s"i = $i, j = $j")
    (i, j)


  def progressiveSnake(current: (Int, Int), previousDir: Option[Direction]): LazyList[((Int, Int), Char)] =
    def nextElement = findNext(current, previousDir)

    if (nextElement.isEmpty) LazyList.empty
    else LazyList.cons((current, maze(current._1)(current._2)), progressiveSnake(nextElement.get._1, nextElement.get._2))

  def findNext(current: (Int, Int), previousDir: Option[Direction]): Option[((Int, Int), Option[Direction])] =
    import Direction._
    val (currentRow, currentColumn) = current
    val currentLetter = maze(current._1)(current._2)

    def isPossible(direction: Direction): Boolean =
      direction match
        case North if currentRow > 1 => maze(currentRow-1)(currentColumn) match
          case '|'|'7'|'F' => true
          case _ => false
        case East if currentColumn < maze(0).length => maze(currentRow)(currentColumn+1) match
          case '-' | 'L' | 'F' => true
          case _ => false
        case South if currentRow < maze.length => maze(currentRow+1)(currentColumn) match
          case 'J' | '|' | 'L' => true
          case _ => false
        case West if currentColumn > 1 => maze(currentRow)(currentColumn-1) match
          case '-' | 'J' | '7' => true
          case _ => false
        case _ => false

    def findIN(directions: Seq[Direction]): Seq[Direction] =
      directions.filter(isPossible(_))

    def isForbidden(nextDirection: Direction): Boolean =
      previousDir match
        case None => false
        case Some(previousDirection) => previousDirection match
          case North if nextDirection == South => true
          case East if nextDirection == West => true
          case South if nextDirection == North => true
          case West if nextDirection == East => true
          case _ => false

    def output(direction: Direction): ((Int, Int), Option[Direction]) =
      direction match
        case North => ((currentRow-1, currentColumn), Some(North))
        case East => ((currentRow, currentColumn+1), Some(East))
        case South => ((currentRow + 1, currentColumn), Some(South))
        case West => ((currentRow, currentColumn-1), Some(West))

    val nextDir = currentLetter match {
      case 'S' if previousDir.isDefined => None
      case value => value match {
        case 'S' => Some(findIN(Seq(North, East, South, West)))
        case '|' => Some(Seq(North, South))
        case '-' => Some(Seq(West, East))
        case '7' => Some(Seq(South, West))
        case 'F' => Some(Seq(East, South))
        case 'L' => Some(Seq(North, East))
        case 'J' => Some(Seq(North, West))
      }
    }

    nextDir match
      case None => None
      case Some(value: Seq[Direction]) => Some(output(value.filterNot(isForbidden(_)).head))

  val partOfTheLoop = progressiveSnake(findStart, None).filterNot(value => value._2 == '-' || value._2 == '|').map :
      case ((x, y), 'L') => ((x, y+1), 'L')
      case ((x, y), 'J') => ((x+1, y+1), 'J')
      case ((x, y), 'F') => ((x, y), 'F')
      case ((x, y), '7') => ((x+1, y), '7')
      case ((x, y), 'S') => ((x, y), 'S')

  val toZipWith = partOfTheLoop.tail :+ partOfTheLoop.head

  //partOfTheLoop.foreach(println)

  val lengthOfQueue = progressiveSnake(findStart, None).length / 2

  val area = partOfTheLoop.zip(toZipWith).foldLeft(0) { (acc, newValue) =>
    val (x1, y1, x2, y2) = (newValue._1._1._1, newValue._1._1._2, newValue._2._1._1, newValue._2._1._2)
    val offsets = Seq((0,0), (0, 1), (1, 0), (1, 1))
    val results = offsets.zip(offsets).map((offset1, offset2) => (x2+offset2._1) * (y1+x2+offset1._2) - ((x1+offset1._1) * (y2+offset2._2))).foldLeft((0,0))((acc, newValue) => (math.min(acc._1, newValue), math.max(acc._2, newValue)))
    val (minAsAbs, maxAsAbs) = (math.abs(results._1), math.abs(results._2))
    if (minAsAbs > maxAsAbs)
      println(s"($x1,$y1) => ($x2,$y2) => ${results._1} = ${acc + results._1}")
      acc + results._1
    else
      println(s"($x1,$y1) => ($x2,$y2) => ${results._2} = ${acc + results._2}")
      acc +  (x1, y1, x2, y2)
  }
  
  /*val area = partOfTheLoop.zip(toZipWith).foldLeft(0d) { (acc, newValue) =>
    val (x1, y1, x2, y2) = (newValue._1._1._1, newValue._1._1._2, newValue._2._1._1, newValue._2._1._2)
    val (letter1, letter2) = (newValue._1._2, newValue._2._2)
    val (x1b, y1b, x2b, y2b) = (letter1, letter2) match
      case ('F', 'J') => (x1, y1, x2-1, y2)
      case ('7', 'L') => (x1+1, y1, x2, y2)
      case _ => (x1, y1, x2, y2)

    println(s"($x1b,$y1b) => ($x2b,$y2b) => ${x2b * y1b - x1b * y2b}")
    acc + (x2b * y1b - x1b * y2b)
  }*/


  println(s" => ${area/2} and ${lengthOfQueue} => ${(area/2)-(lengthOfQueue)*2}")

  /*val candidateToSolutionPart2 = for j <- 0 until maze(0).length
                                     i <- 0 until maze.length
                                 yield
                                   if (partOfTheLoop.contains((i,j)))
                                     loggerAOCPart2.trace(s"excluding $i, $j")
                                     None
                                   else
                                     Some(i, j)*/

  //loggerAOCPart2.trace(s"${partOfTheLoop.length} : ${candidateToSolutionPart2.length}, from ${maze(0).length} and ${maze.length} max")

  //loggerAOCPart2.debug(s"${candidateToSolutionPart2.filterNot(_.isEmpty).length} possible")

  lengthOfQueue

