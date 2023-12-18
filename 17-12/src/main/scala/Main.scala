import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._

import java.time.Duration
import java.time.Instant
// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

import Dir._

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching 17-12")
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

    val container1 = Container(lines)
    println(container1)
    println("<----------------------->")
    val result = container1.afterDijkstra(Position(0, 0))
    println(s"resultat : ${result.map(_.position).map((position) => container1.data(position.row)(position.col)).sum}")
    println(container1.display(result))

    /*pathLazyList(List(Path(container1, List(Position(0,0)))), 0).last match
      case Some(path) => println(s"Heat is ${path.heat}") ; println(container1.display(path))
      case None => println(s"Not found")*/

    val (result1, result2) = ("", "")



    (s"${result1}", s"${result2}")

class Container(input: Seq[String]):
  val width = input(0).length
  val heigh = input.length
  val data: Array[Array[Int]] = input.toArray.map(_.toArray.map(_.asDigit))

  def fromNodes(data: Array[Array[Node]]): Container =
    val tempo = data.map(_.map(_.value.toInt).mkString)
    Container(data.map(_.map(_.value.toInt).mkString))

  def from(position: Position): List[Int] =
    (for row <- position.row until heigh
        col <- position.col until width
    yield
        data(row)(col)
      ).toList

  def display(path: Path): String =
    val tempo = data.map(_.map(_.toString.head).clone())
    path.passedThroughLastToFirst.foreach:
      position => tempo(position.row)(position.col) = '#'
    asString(tempo)

  def display(listOfNodes: List[Node]): String =
    val tempo = data.map(_.map(_.toString.head).clone())
    listOfNodes.map(_.position).foreach:
      position => tempo(position.row)(position.col) = '#'
    asString(tempo)

  def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  override def toString =
    asString(data.map(_.map(_.toString.head)))

  def afterDijkstra(fromPosition: Position): List[Node] =
    val working: Array[Array[Node]] = Array.tabulate(heigh, width)((row, col) => Node(false, Long.MaxValue, Position(row, col), None))
    working(0)(0) = working(0)(0).copy(value = 0)
    applyDijkstra(working, this)

def applyDijkstra(working: Array[Array[Node]], distances: Container): List[Node] =
  def foundNodes(fromNode: Node): List[Node] =
    fromNode.prev match
      case Some(value) => fromNode :: foundNodes(value)
      case None => Nil

  def markVisited(node: Node): Unit =
    val Position(row, col) = node.position
    working(row)(col) = node.copy(visited = true)

  def updateValueAndPrevious(targetNode: Node, newValue: Long, fromNode: Node): Unit =
    val Position(row, col) = targetNode.position
    working(row)(col) = targetNode.copy(value = newValue, prev = Some(fromNode))

  def previousDirs(node: Node): List[Dir] =
    val previousNodes = foundNodes(node).take(4)
    previousNodes.length > 2 match
      case false => Nil
      case true =>
        previousNodes.zip(previousNodes.tail).map((node1, node2) => (node1.position, node2.position)).map:
          case (Position(row1, col1), Position(row2, col2)) if row1 == row2 =>
            col1 > col2 match
              case true => LeftToRight
              case false => RightToLeft
          case (Position(row1, col1), Position(row2, col2)) =>
            row1 > row2 match
              case true => UpToDown
              case false => DownToUp
        .take(3)

  def notAuthorized(node: Node): Option[Node] =
    val previousDirList = previousDirs(node)
    val Position(row, col) = node.position
    previousDirList.length match
      case 3 =>
        val previous = previousDirList
        previous.tail.filter(_ == previous.head).length match
          case 2 => previous.head match
            case UpToDown if row < distances.width - 1 => Some(working(row+1)(col))
            case DownToUp if row > 0 => Some(working(row-1)(col))
            case LeftToRight if col < distances.heigh - 1 => Some(working(row)(col+1))
            case RightToLeft if col > 0 => Some(working(row)(col-1))
            case _ => None
          case _ => None
      case _ => None

  def findNotVisitedNeightBoors(node: Node): List[Node] =
    (node.position match
      case Position(0, 0) => List(working(0)(1), working(1)(0))
      case Position(0, col) if col == distances.width - 1 => List(working(0)(col-1), working(1)(col))
      case Position(row, 0) if row == distances.heigh - 1 => List(working(row)(1), working(row-1)(0))
      case Position(row, col) if col == distances.width - 1 && row == distances.heigh - 1 => List(working(row-1)(col), working(row)(col-1))
      case Position(row, col) if row == distances.heigh - 1 => List(working(row-1)(col), working(row)(col-1), working(row)(col+1))
      case Position(row, col) if col == distances.width - 1 => List(working(row-1)(col), working(row+1)(col), working(row)(col))
      case Position(0, col) => List(working(0)(col-1), working(0)(col+1), working(1)(col))
      case Position(row, 0) => List(working(row-1)(0), working(row+1)(0), working(row)(1))
      case Position(row, col) => List(working(row-1)(col), working(row+1)(col), working(row)(col-1), working(row)(col+1))
      ).filterNot(_.visited).filterNot(curentNode => notAuthorized(node).map(_ == curentNode).getOrElse(false))


  val bestNotVisitedYet = working.flatten.filterNot(_.visited).sortBy(_.value).head
  //println(s" => $bestNotVisitedYet")

  markVisited(bestNotVisitedYet)

  bestNotVisitedYet.value >= working(distances.heigh-1)(distances.width-1).value match
    case true => ()
    case false =>
      findNotVisitedNeightBoors(bestNotVisitedYet).foreach { currentNeighborNode =>
        val Position(itsRow, itsCol) = currentNeighborNode.position
        val possibleNewValue: Long = bestNotVisitedYet.value + distances.data(itsRow)(itsCol)
        possibleNewValue <= currentNeighborNode.value match
          case true => updateValueAndPrevious(currentNeighborNode, possibleNewValue, bestNotVisitedYet)
          case false => ()
      }

  working.flatten.filterNot(_.visited).isEmpty match
    case true => foundNodes(working(distances.heigh-1)(distances.width-1).prev.get)
    case false => applyDijkstra(working, distances)


case class Node(visited: Boolean, value: Long, position: Position, prev: Option[Node]) :
  override def toString: String = s"(${position.row},${position.col}) , $visited, $value"

case class Position(row: Int, col: Int)

class Path(val container: Container, val passedThroughLastToFirst: List[Position]):
  val isSuccess = passedThroughLastToFirst.head == Position(container.heigh-1, container.width-1)
  def minimalDistanceTilEnd(position: Position): Long =
    container.from(passedThroughLastToFirst.head).sorted.take(minimalLengthOfPathTillEnd(position)).sum
  def minimalLengthOfPathTillEnd(position: Position): Int =
    (position.col - container.width - 1) + (position.row - container.heigh - 1)
  val currentHeat: Long = passedThroughLastToFirst.filterNot(position => position == Position(0, 0)).foldLeft(0l) {(acc, newPosition) => acc + container.data(newPosition.row)(newPosition.col) }
  val heat: Long = currentHeat + minimalDistanceTilEnd(passedThroughLastToFirst.head)
  def previousDirs: List[Dir] =
    passedThroughLastToFirst.zip(passedThroughLastToFirst.tail).map :
        case (Position(row1, col1), Position(row2, col2)) if row1 == row2 =>
          col1 > col2 match
            case true => LeftToRight
            case false => RightToLeft
        case (Position(row1, col1), Position(row2, col2)) =>
          row1 > row2 match
            case true => UpToDown
            case false => DownToUp
    .take(3)

  def notAuthorized: Option[Dir] =
    previousDirs.length match
      case 3 =>
        val previous = previousDirs
        previous.tail.filter(_ == previous.head).length match
          case 2 => Some(previous.head)
          case _ => None
      case _ => None

  def possibleNextDirs: List[Dir] =
    val currentPosition = passedThroughLastToFirst.head
    val impossible = currentPosition match
      case Position(0, 0) => List(DownToUp, RightToLeft)
      case Position(0, col) if col == container.width - 1 => List(DownToUp, LeftToRight)
      case Position(row, 0) if row == container.heigh - 1 => List(UpToDown, RightToLeft)
      case Position(row, col) if col == container.width - 1 && row == container.heigh - 1 => List(UpToDown, LeftToRight)
      case Position(row, _) if row == container.heigh - 1 => List(UpToDown)
      case Position(_, col) if col == container.width - 1 => List(LeftToRight)
      case Position(0, _) => List(DownToUp)
      case Position(_, 0) => List(RightToLeft)
      case _ => Nil

    val result = Dir.values.filterNot(impossible.contains(_)).filterNot(current => notAuthorized.map(_ == current).getOrElse(false)).toList
    //println(result)
    result

  def next(heatLimit: Long): List[Path] =
    heat match
      case value if value <= heatLimit => goNextIfNotTooHot()
      case _ => List(this)

  def goNextIfNotTooHot(): List[Path] =
    val currentPosition = passedThroughLastToFirst.head
    possibleNextDirs.map { currentNextDir =>
      currentNextDir match
        case UpToDown => addIfPossible(currentPosition.copy(row = currentPosition.row+1))
        case DownToUp => addIfPossible(currentPosition.copy(row = currentPosition.row - 1))
        case LeftToRight => addIfPossible(currentPosition.copy(col = currentPosition.col + 1))
        case RightToLeft => addIfPossible(currentPosition.copy(col = currentPosition.col - 1))
    }.filter(_.isDefined).map(_.get)

  def addIfPossible(position: Position): Option[Path] =
    passedThroughLastToFirst.find(_ == position).isDefined match
      case true => None
      case false => Some(Path(container, position +: passedThroughLastToFirst))

  override def toString: String = s"${passedThroughLastToFirst.mkString(">")} [${this.heat}] => ${isSuccess}"

def pathLazyList(paths: List[Path], step: Int): LazyList[Option[Path]] =
  val bestSuccessfulTemp: Option[Long] = paths.filter(_.isSuccess).map(_.heat).minOption
  val lowerTemp = paths.map(_.heat).min
  val newPath = paths.filterNot(_.isSuccess).flatMap { path =>
    //if (step > 40)
      //println(s"[$step] - $path - should be below ${lowerTemp}")
    path.next(lowerTemp)
  }

  val stillPossible = bestSuccessfulTemp match
    case Some(value) => newPath.filterNot(_.heat >= value) ::: paths.filter(_.isSuccess)
    case None => newPath

  paths.filter(_.isSuccess).length >= 1 match
    case true if stillPossible.filterNot(_.isSuccess).length == 0 => LazyList.empty
    case _ => LazyList.cons(stillPossible.sortBy(_.heat).find(_.isSuccess).headOption, pathLazyList(stillPossible, step + 1))

enum Dir:
  case UpToDown, DownToUp, LeftToRight, RightToLeft
