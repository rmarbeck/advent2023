import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._

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

    val container = Container(lines)

    println(container)
    //println(container.display(container.solvePart2.getPositions))

    println(container.solvePart2do.max)

    val (result1, result2) = ("", "")



    (s"${result1}", s"${result2}")

case class Coords(row: Int, col: Int):
  def fromUp(prevCoords: Coords): Boolean =
    (prevCoords.row, prevCoords.col) match
      case (r, c) if r == row + 1 => true
      case _ => false

  def fromDown(prevCoords: Coords): Boolean =
  (prevCoords.row, prevCoords.col) match
    case (r, c) if r == row - 1 => true
    case _ => false

  def fromLeft(prevCoords: Coords): Boolean =
    (prevCoords.row, prevCoords.col) match
      case (r, c) if c == col + 1 => true
      case _ => false

  def fromRight(prevCoords: Coords): Boolean =
      (prevCoords.row, prevCoords.col) match
        case (r, c) if c == col - 1 => true
        case _ => false

  def nextPossibleMovesPart2(from: Option[Coords])(implicit container: Container): List[Coords] =
    val firstPass = List((0, 1), (0, -1), (-1, 0), (1, 0)).map(coords => (row + coords._1, col + coords._2)).filter: (currentRow, currentCol) =>
      container.data.isDefinedAt(currentRow) && container.data(currentRow).isDefinedAt(currentCol)
    .filter(current => container.data(current._1)(current._2) != '#')
    .filterNot(current => from.map(found => found.row == current._1 && current._2 == found.col).getOrElse(false))
    .map(valid => Coords(valid._1, valid._2))

    firstPass


object Coords:
  def from(position: Position) = Coords(position.row, position.col)

case class Path(coordsList: List[Coords], length: Int = 0):
  def getPreviousLast: Option[Coords] = coordsList.drop(1).headOption
  def getLast: Coords = coordsList.head
  def fromStart: List[Coords] = coordsList.reverse

  def addPosition(coords: Coords)(implicit coordsHolder: CoordsHolder): Either[Option[Path], Path] =
    isArrived(coords)(coordsHolder.container) match
      case true => Right(Path(coords +: coordsList, length + 1))
      case _ =>
        isAlreadyPassedThrough(coords) || isTooWeak(coords) match
          case true => Left(None)
          case false => Left(Some(Path(coords +: coordsList, length + 1)))

  private def isArrived(coords: Coords)(implicit container: Container): Boolean =
    val result = coords.row == container.height - 1 && coords.col == container.width - 2
    //if (result)
      //println(container.display(positions))
      //println(s"arrived through ${this}")
    result
  private def isAlreadyPassedThrough(coords: Coords): Boolean =
    val result = coordsList.map:
      case Coords(row, col) => coords.row == row && coords.col == col
    .find(_ == true).isDefined

    //if (result)
      //println(container.display(positions))
      //println(s"passed through ${coords}")
    result

  private def isTooWeak(coords: Coords)(implicit coordsHolder: CoordsHolder): Boolean =
    false
    /*val result = coordsList match
      case Nil => false
      case head :: Nil => false
      case head :: tail =>
        tail.head match
          case previous if head.fromUp(previous) => coordsHolder.bestFromUp(head.row)(head.col) > length + 1
          case previous if head.fromDown(previous) => coordsHolder.bestFromDown(head.row)(head.col) > length + 1
          case previous if head.fromLeft(previous) => coordsHolder.bestFromLeft(head.row)(head.col) > length + 1
          case previous => coordsHolder.bestFromRight(head.row)(head.col) > length + 1

    //if (result)
      //println(container.display(positions))
      //println(s"too weak !!! ${this} ${coordsHolder}")
      //println(s"too weak !!! on $coords")
    result*/
  override def toString: String = s"{${this.length}} - ${coordsList.map(cur => s"[${cur.row}, ${cur.col}]").mkString(" ")}"



class CoordsHolder(val container: Container):
  val bestFromDown, bestFromUp, bestFromLeft, bestFromRight = Array.fill(container.height, container.width) (0)
  def best(row: Int)(col: Int) = max(max(max(bestFromUp(row)(col), bestFromDown(row)(col)), bestFromLeft(row)(col)), bestFromRight(row)(col))

  def addSuccessPath(successPath: Path): Unit =
    val successPathValues = successPath.fromStart
    successPathValues.zip(successPathValues.tail).zipWithIndex.foreach:
      case ((coorsdHead, coordsNext), index) if coordsNext.fromUp(coorsdHead) =>
        bestFromUp(coordsNext.row)(coordsNext.col) = max(index + 1, bestFromUp(coordsNext.row)(coordsNext.col))
      case ((coorsdHead, coordsNext), index) if coordsNext.fromDown(coorsdHead) =>
        bestFromDown(coordsNext.row)(coordsNext.col) = max(index + 1, bestFromDown(coordsNext.row)(coordsNext.col))
      case ((coorsdHead, coordsNext), index) if coordsNext.fromLeft(coorsdHead) =>
        bestFromLeft(coordsNext.row)(coordsNext.col) = max(index + 1, bestFromLeft(coordsNext.row)(coordsNext.col))
      case ((coorsdHead, coordsNext), index) if coordsNext.fromRight(coorsdHead) =>
        bestFromRight(coordsNext.row)(coordsNext.col) = max(index + 1, bestFromRight(coordsNext.row)(coordsNext.col))
      case _ =>
        throw Exception("Not supported")

    //println(this)

  override def toString: String =
    val tempo = Array.tabulate(container.height, container.width) { (row, col) =>
      bestFromLeft(row)(col)
    }
    tempo.map(_.mkString("\t")).mkString("\n")

class PositionsHolder(val container: Container):
  val positions = Array.tabulate(container.height, container.width) {(row, col) =>
    Position(row, col, List(), 0)(container)
  }

  def addPosition(position: Position) =
    positions(position.row)(position.col).bestScore match
      case value if value > position.bestScore => ()
      case _ => positions(position.row)(position.col) = position

  def addPositionPart2(position: Position) =
    positions(position.row)(position.col).bestScore match
      case value if value > position.bestScore =>
        positions(position.row)(position.col) = Position(position.row, position.col, positions(position.row)(position.col).previous ::: position.previous, value)(container)
      case _ =>
        positions(position.row)(position.col) = Position(position.row, position.col, positions(position.row)(position.col).previous ::: position.previous, position.bestScore)(container)

  def addPositionAndUpdate(position: Position): Position =
    positions(position.row)(position.col) = Position(position.row, position.col, positions(position.row)(position.col).previous ::: position.previous, max(position.bestScore, positions(position.row)(position.col).bestScore))(container)
    positions(position.row)(position.col)

  def addSucessPath(sucessPath: Path): Unit =
    sucessPath.fromStart.zipWithIndex.foreach:
      case (coords, index) => positions(coords.row)(coords.col) = Position(coords.row, coords.col, List(), max(positions(coords.row)(coords.col).bestScore, index))(container)


  def getPositions: List[Position] =
    positions.flatten.filter(_.bestScore != 0).toList

  override def toString: String = positions.flatten.map(_.bestScore).maxOption.getOrElse(0).toString

class Position(val row: Int, val col: Int, var previous: List[Coords], var bestScore: Int)(implicit container: Container):
  def singleNext(slope: Char): List[Position] =
    List(
      slope match
        case '>' => Position(row, col+1, List(Coords.from(this)), bestScore + 1)
        case '<' => Position(row, col-1, List(Coords.from(this)), bestScore + 1)
        case 'v' => Position(row+1, col, List(Coords.from(this)), bestScore + 1)
        case '^' => Position(row-1, col, List(Coords.from(this)), bestScore + 1)
    )
  def nextPossibleMoves: List[Position] =
    container.data(row)(col) match
      case value @  ('>' | '<' | 'v' | '^') => singleNext(value)
      case _ =>
        val firstPass = List((0,1),(0,-1),(-1,0),(1,0)).map(coords => (row + coords._1, col + coords._2)).filter: (currentRow, currentCol) =>
          container.data.isDefinedAt(currentRow) && container.data(currentRow).isDefinedAt(currentCol)
        .filter(current => container.data(current._1)(current._2) != '#')
        .filterNot(current => this.previous.find(prev => (prev.row, prev.col) == current).isDefined)
        .map(valid => Position(valid._1, valid._2, List(Coords.from(this)), this.bestScore+1))

        firstPass.filter:
          case Position(rowNext, colNext, _, _) if rowNext == row + 1 => ".v".contains(container.data(rowNext)(colNext))
          case Position(rowNext, colNext, _, _) if rowNext == row - 1 => ".^".contains(container.data(rowNext)(colNext))
          case Position(rowNext, colNext, _, _) if colNext == col + 1 => ".>".contains(container.data(rowNext)(colNext))
          case Position(rowNext, colNext, _, _) if colNext == col - 1 => ".<".contains(container.data(rowNext)(colNext))

  def nextPossibleMovesPart2: List[Coords] =
    val firstPass = List((0, 1), (0, -1), (-1, 0), (1, 0)).map(coords => (row + coords._1, col + coords._2)).filter: (currentRow, currentCol) =>
      container.data.isDefinedAt(currentRow) && container.data(currentRow).isDefinedAt(currentCol)
    .filter(current => container.data(current._1)(current._2) != '#')
    .filterNot: current =>
      val result = this.previous.find(prev => (prev.row, prev.col) == current).isDefined
      result
    .map(valid => Coords(valid._1, valid._2))

    firstPass

  def nextPossibleMovesPart2b: List[Position] =
    val firstPass = List((0, 1), (0, -1), (-1, 0), (1, 0)).map(coords => (row + coords._1, col + coords._2)).filter: (currentRow, currentCol) =>
      container.data.isDefinedAt(currentRow) && container.data(currentRow).isDefinedAt(currentCol)
    .filter(current => container.data(current._1)(current._2) != '#')
    .filterNot:current =>
      val result = this.previous.find(prev => (prev.row, prev.col) == current).isDefined
      result
    .map(valid => Position(valid._1, valid._2, List(Coords.from(this)), this.bestScore + 1))

    firstPass


  override def toString: String = s"[$row,$col] {$previous} - $bestScore"

object Position:
  def unapply(position: Position) : (Int, Int, List[Coords], Int) =
    (position.row, position.col, position.previous, position.bestScore)

class Container(input: Seq[String]):
  val width = input(0).length
  val height: Int = input.length
  val data: Array[Array[Char]] = input.toArray.map(_.toArray)

  def display(path: Path) =
    val tempo = data.map(_.clone())
    path.fromStart.foreach:
      position => tempo(position.row)(position.col) = 'O'
    asString(tempo)

  def display(positions: List[Position]) =
    val tempo = data.map(_.clone())
    positions.foreach:
      position => tempo(position.row)(position.col) = 'O'
    asString(tempo)

  def asString(data: Array[Array[Char]]) =
    data.map(_.mkString(" ")).mkString("\n")

  def findStart: Position =
    Position(0, data(0).indexOf('.'), List(), 0)(this)

  def solve = explore(List(findStart), PositionsHolder(this), 0)

  def solvePart2do = solvePart2(List(Path(List(Coords.from(findStart)))), List(), 0)(CoordsHolder(this))

  override def toString: String = asString(this.data)

def explore(toExplore: List[Position], explored: PositionsHolder, steps: Int): PositionsHolder =
  toExplore match
    case Nil => explored
    case head :: tail =>
      val nextMoves = head.nextPossibleMoves
      nextMoves.foreach(explored.addPosition)
      explore(tail ::: nextMoves, explored, steps + 1)


def solvePart2(pathsToExplore: List[Path], values: List[Int], steps: Int)(implicit coordsHolder: CoordsHolder): List[Int] =
  if (steps == 50000000)
    println("sopped by step limit")
    values
  else
    //println(s"${pathsToExplore.length}")
    pathsToExplore match
      case Nil => values
      case head :: tail =>
        val nextMoves = head.getLast.nextPossibleMovesPart2(head.getPreviousLast)(coordsHolder.container)

        val pathsManaged = nextMoves.map(head.addPosition(_)(coordsHolder))
        val solutions = pathsManaged.partitionMap(identity)
        val finalSolutions = solutions._2
        finalSolutions.foreach: currentPath =>
          if (values.length > 1)
            println(s"Final path found ${max(values.max, currentPath.length)}")
          coordsHolder.addSuccessPath(currentPath)

        val pathsToContinue = solutions._1.flatten
        //println(s"$head <-> $nextMoves => ${finalSolutions.sum} <= $pathsToContinue ")
        //pathsToContinue.map(currentPath => positionHolder.addPositionAndUpdate(currentPath.getLast))

        solvePart2(tail ::: pathsToContinue, values ::: finalSolutions.map(_.length), steps + 1)

def solvePart2b(pathsToExplore: List[Path], values: List[Int], steps: Int)(implicit positionHolder: PositionsHolder): List[Int] =
  if (steps == 50000000)
    println("sopped by step limit")
    values
  else
    //println(s"${pathsToExplore.length}")
    values
    /*pathsToExplore match
      case Nil => values
      case head :: tail =>
        val nextMoves = head.getLast.nextPossibleMovesPart2

        val pathsManaged = nextMoves.map(head.addPosition(_)(positionHolder))
        val solutions = pathsManaged.partitionMap(identity)
        val finalSolutions = solutions._2
        finalSolutions.foreach(positionHolder.addSucessPath(_))
        val pathsToContinue = solutions._1.flatten
        //println(s"$head <-> $nextMoves => ${finalSolutions.sum} <= $pathsToContinue ")
        //pathsToContinue.map(currentPath => positionHolder.addPositionAndUpdate(currentPath.getLast))

        solvePart2(pathsToContinue ::: tail, values ::: finalSolutions.map(_.length), steps + 1)*/


def explorePart2(toExplore: List[Position], explored: PositionsHolder, steps: Int): PositionsHolder =
  if (steps == 100000)
    explored
  else
    explored
    /*toExplore match
      case Nil => explored
      case head :: tail =>
        val nextMoves = head.nextPossibleMovesPart2
        println(s"from $head to $nextMoves")
        val nextMovesUpdated = nextMoves.map(explored.addPositionAndUpdate(_))
        explorePart2(tail ::: nextMovesUpdated, explored, steps + 1)*/
