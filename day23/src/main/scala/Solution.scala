import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given forest: Forest = Forest.from(inputLines)
    val resultPart1 = {
      /*val crossRoads = findCrossRoads(part2 = false)

      val summits = crossRoads.map:
        currentCrossRoad =>
          val nextSummits =
            crossRoads.flatMap:
              nextCrossRoad =>
                nextCrossRoad.from.find(_._1.position == currentCrossRoad.position).map:
                  (_, distance) => (Summit.toName(nextCrossRoad), distance)

          Summit(Summit.toName(currentCrossRoad), nextSummits)*/

      val summits = findSummits(part2 = false)//.sortBy(-_.nexts.length).distinctBy(_.name)

      summits.foreach(println)

      given summitsHolder: SummitsHolder = SummitsHolder(summits)

      val result = topologicalSort(summitsHolder.byName("0-1")).values

      val resultWithData = result.zipWithIndex.map:
        case (currentSummit, 0) =>
          WithData[Summit](currentSummit, 1)
        case (currentSummit, _) =>
          WithData[Summit](currentSummit)

      count(resultWithData)
    }

    val resultPart2 = {
      val crossRoads = findCrossRoads(part2 = true)

      val summits = crossRoads.map:
        currentCrossRoad =>
          val nextSummits =
            crossRoads.flatMap:
              nextCrossRoad =>
                nextCrossRoad.from.find(_._1.position == currentCrossRoad.position).map:
                  (_, distance) => (Summit.toName(nextCrossRoad), distance)

          Summit(Summit.toName(currentCrossRoad), nextSummits)

      val summitsWithBacklinks = summits.map:
        currentSummit =>
          val backlinks = summits.filterNot(_ == currentSummit).flatMap:
            otherSummit =>
              otherSummit.nexts.find:
                (nextDistanceToSummit, _) => nextDistanceToSummit == currentSummit.name
              .map(otherSummitThatContainsCurrentInNext => (otherSummit.name, otherSummitThatContainsCurrentInNext._2))
          Summit(currentSummit.name, (currentSummit.nexts ::: backlinks).distinct)

      given summitsHolder2: SummitsHolder = SummitsHolder(summitsWithBacklinks)
      val endName = Summit.toName(Position(forest.height-1, forest.width-2))
      val result2 = countPart2(summitsHolder2.byName("0-1"), Nil, summitsHolder2.byName(endName))
      result2.flatMap(_._1).get
    }



    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

case class Position(row: Int, col: Int):
  lazy val north = this.copy(row = row - 1)
  lazy val south = this.copy(row = row + 1)
  lazy val west = this.copy(col = col - 1)
  lazy val east = this.copy(col = col + 1)

  lazy val around = List(north, south, west, east)

  lazy val sorter: Int = - (col + row)

  override def toString: String = s"$row,$col"

enum TypeOfLocation:
  case Path, Tree, SlopeUp, SlopeDown, SlopeLeft, SlopeRight

export TypeOfLocation.*

type SummitName = String

type DistanceToCrossRoad = (CrossRoad, Int)
type DistanceToSummit = (SummitName, Int)


case class Summit(name: SummitName, nexts: List[DistanceToSummit]):
  lazy val position: Position = Summit.fromName(name)
  //override def toString: String = s"[$name]"
  override def toString: String = s"\n[$name] ${nexts.map(current => s"\n  |\n   -> ${current._1} (${current._2})").mkString}"

object Summit:
  def toName(crossRoad: CrossRoad): SummitName = toName(crossRoad.position)
  def toName(position: Position): SummitName = s"${position.row}-${position.col}"
  def fromName(name: SummitName): Position =
    name match
      case s"${row}-${col}" => Position(row.toInt, col.toInt)

case class SummitsHolder(allSummits: Seq[Summit]):
  val byName: Map[String, Summit] = allSummits.map(current => current.name -> current).toMap
  def nextOf(summit: Summit): List[Summit] =
    summit.nexts.map(_._1).map(byName)
  def distanceBetween(first: Summit, second: Summit): Int =
    first.nexts.find(current => current._1 == second.name).map(_._2).get


case class CrossRoad(position: Position, from: List[DistanceToCrossRoad] = Nil):
  override def toString: String = s"[${position.row}, ${position.col}] => ${from.map(_._1.position).mkString(" <-> ")}"

@tailrec
def findNext(crossRoadsToExplore: List[CrossRoad], nextPosition: Option[Position], currentSteps: Int, alreadyExploredPositions: List[Position], alreadyExploredCrossRoads: List[CrossRoad], part2: Boolean = false)(using forest: Forest): List[CrossRoad] =
  def goNext =
    part2 match
      case true => forest.nextPart2
      case false => forest.nextPart1
  def nextPositionsAuthorized(from: Position) =
      goNext(from).filterNot(alreadyExploredPositions.contains).filterNot(_ == crossRoadsToExplore.head.position)
  crossRoadsToExplore match
    case Nil => alreadyExploredCrossRoads
    case head :: tail =>
      nextPosition match
        case None =>
          nextPositionsAuthorized(head.position) match
            case Nil => findNext(tail, None, 1, alreadyExploredPositions, head +: alreadyExploredCrossRoads, part2)
            case newNextPosition :: _ => findNext(crossRoadsToExplore, Some(newNextPosition), 0, alreadyExploredPositions, alreadyExploredCrossRoads, part2)
        case Some(nextPos) =>
          forest.isACrossRoad(nextPos) match
            case true =>
              crossRoadsToExplore.find(_.position == nextPos) match
                case Some(previouslyFound) =>
                  val newCrossRoad = previouslyFound.copy(from = (head, currentSteps) +: previouslyFound.from)
                  val (newCrossRoads, newAlreadyExploredCrossRoads, newNextPosition) =
                    nextPositionsAuthorized(head.position) match
                      case Nil =>
                        val crossRoadsWithoutHead = tail.filterNot(_ == previouslyFound) :+ newCrossRoad
                        val newNextPosition = nextPositionsAuthorized(crossRoadsWithoutHead.head.position).headOption
                        (crossRoadsWithoutHead, head +: alreadyExploredCrossRoads, newNextPosition)
                      case possibleNexts => (crossRoadsToExplore.filterNot(_ == previouslyFound) :+ newCrossRoad, alreadyExploredCrossRoads, possibleNexts.headOption)

                  findNext(newCrossRoads, newNextPosition, 1, alreadyExploredPositions, newAlreadyExploredCrossRoads, part2)

                case None =>
                  val newCrossRoad = CrossRoad(nextPos, List((head, currentSteps)))
                  val (newCrossRoads, newAlreadyExploredCrossRoads, newNextPosition) =
                    nextPositionsAuthorized(head.position) match
                      case Nil =>
                        val crossRoadsWithoutHead = tail :+ newCrossRoad
                        val newNextPosition = nextPositionsAuthorized(crossRoadsWithoutHead.head.position).headOption
                        (crossRoadsWithoutHead, head +: alreadyExploredCrossRoads, newNextPosition)
                      case possibleNexts => (crossRoadsToExplore :+ newCrossRoad, alreadyExploredCrossRoads, possibleNexts.headOption)

                  findNext(newCrossRoads, newNextPosition, 1, alreadyExploredPositions, newAlreadyExploredCrossRoads, part2)
            case false => findNext(crossRoadsToExplore, nextPositionsAuthorized(nextPos).headOption, currentSteps+1, nextPos +: alreadyExploredPositions, alreadyExploredCrossRoads, part2)


@tailrec
def findNextSummit(summitsToExplore: List[Summit], nextPosition: Option[Position], currentSteps: Int, alreadyExploredPositions: List[Position], alreadyExploredSummits: List[Summit], part2: Boolean = false)(using forest: Forest): List[Summit] =
  def goNext =
    part2 match
      case true => forest.nextPart2
      case false => forest.nextPart1
  def nextPositionsAuthorized(from: Position) =
    goNext(from).filterNot(alreadyExploredPositions.contains).filterNot(_ == summitsToExplore.head.position)
  summitsToExplore match
    case Nil => alreadyExploredSummits
    case head :: tail =>
      nextPosition match
        case None =>
          nextPositionsAuthorized(head.position) match
            case Nil =>
              if (alreadyExploredSummits.map(_.name).contains(head.name))
                findNextSummit(tail, None, 1, alreadyExploredPositions, alreadyExploredSummits, part2)
              else
                findNextSummit(tail, None, 1, alreadyExploredPositions, head +: alreadyExploredSummits, part2)
            case newNextPosition :: _ => findNextSummit(summitsToExplore, Some(newNextPosition), 0, alreadyExploredPositions, alreadyExploredSummits, part2)
        case Some(nextPos) =>
          forest.isACrossRoad(nextPos) match
            case true =>
              summitsToExplore.find(_.position == nextPos) match
                case Some(previouslyFound) =>
                  val updatedHead = head.copy(nexts = (previouslyFound.name, currentSteps) +: head.nexts)
                  val (newSummits, newAlreadyExploredSummits, newNextPosition) =
                    nextPositionsAuthorized(head.position) match
                      case Nil =>
                        val newNextPosition = nextPositionsAuthorized(tail.head.position).headOption
                        (tail, updatedHead +: alreadyExploredSummits, newNextPosition)
                      case possibleNexts => (updatedHead +: tail, alreadyExploredSummits, possibleNexts.headOption)

                  findNextSummit(newSummits, newNextPosition, 1, alreadyExploredPositions, newAlreadyExploredSummits, part2)

                case None =>
                  val newSummit = Summit(Summit.toName(nextPos), Nil)
                  val updatedCurrentHead = head.copy(nexts = (newSummit.name, currentSteps) +: head.nexts)
                  val (newSummits, newAlreadyExploredSummits, newNextPosition) =
                    nextPositionsAuthorized(head.position) match
                      case Nil =>
                        val summitsWithoutHead = tail :+ newSummit
                        val newNextPosition = nextPositionsAuthorized(summitsWithoutHead.head.position).headOption
                        (summitsWithoutHead, updatedCurrentHead +: alreadyExploredSummits, newNextPosition)
                      case possibleNexts =>
                        val newSummits = updatedCurrentHead +: (tail :+ newSummit)
                        (newSummits, alreadyExploredSummits, possibleNexts.headOption)

                  findNextSummit(newSummits, newNextPosition, 1, alreadyExploredPositions, newAlreadyExploredSummits, part2)
            case false => findNextSummit(summitsToExplore, nextPositionsAuthorized(nextPos).headOption, currentSteps+1, nextPos +: alreadyExploredPositions, alreadyExploredSummits, part2)

def findCrossRoads(part2: Boolean)(using forest: Forest): List[CrossRoad] =
  val start = Position(0,1)
  val initialCrossRoad = CrossRoad(start)
  findNext(List(initialCrossRoad), Some(start), 0, Nil, Nil, part2)

def findSummits(part2: Boolean)(using forest: Forest): List[Summit] =
  val start = Position(0,1)
  val initialSummit = Summit(Summit.toName(start), Nil)
  findNextSummit(List(initialSummit), Some(start), 0, Nil, Nil, part2)



case class Forest(places: Array[Array[TypeOfLocation]]):
  lazy val height = places.length
  lazy val width = places(0).length
  def nbOfSlopes: Int = places.flatten.count:
    case SlopeUp | SlopeDown | SlopeLeft | SlopeRight => true
    case _ => false

  private def isReachable(position: Position): Boolean =
    val Position(row, col) = position
    places.isDefinedAt(row) && places(row).isDefinedAt(col) && places(row)(col) != Tree

  private def isReachableFrom(position: Position, from: Position): Boolean =
    val Position(row, col) = position
    if (isReachable(position))
      (places(row)(col), row - from.row, col - from.col) match
      case (Path, _, _) => true
      case (SlopeUp, rowChange, _) if rowChange < 0 => true
      case (SlopeDown, rowChange, _) if rowChange > 0 => true
      case (SlopeLeft, _, colChange) if colChange < 0 => true
      case (SlopeRight, _, colChange) if colChange > 0=> true
      case _ => false
    else
      false

  def nextPart1(position: Position): List[Position] = position.around.filter(isReachableFrom(_, position))
  def nextPart2(position: Position): List[Position] = position.around.filter(isReachable)

  def isACrossRoad(position: Position): Boolean = position.around.filter(isReachable).length > 2 || (position.row == height -1 && position.col == width - 2)

object Forest:
  def from(inputLines: Seq[String]): Forest =
    val places = inputLines.toArray.map:
      line => line.toCharArray.map:
        case '.' => Path
        case '#' => Tree
        case '^' => SlopeUp
        case 'v' => SlopeDown
        case '<' => SlopeLeft
        case '>' => SlopeRight
    Forest(places)
