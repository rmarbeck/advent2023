import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given forest: Forest = Forest.from(inputLines)
    val resultPart1 = {
      val summits = findSummits(part2 = false)
      given summitsHolder: SummitsHolder = SummitsHolder(summits)

      val start = summitsHolder.byPosition(forest.startPosition)
      val sortedGraph = topologicalSort(start)

      countTopologicallySorted(sortedGraph)
    }

    val resultPart2 = {
      val summits = findSummits(part2 = true)

      given summitsHolder2: SummitsHolder = SummitsHolder(summits).makeFullDuplexConnexions
      val start = summitsHolder2.byPosition(forest.startPosition)
      val end = summitsHolder2.byPosition(forest.endPosition)
      countDFS(start, end).get
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

enum TypeOfLocation:
  case Path, Tree, SlopeUp, SlopeDown, SlopeLeft, SlopeRight

export TypeOfLocation.*

type SummitName = String

type DistanceToSummit = (SummitName, Int)

case class Summit(name: SummitName, nexts: List[DistanceToSummit]):
  lazy val position: Position = Summit.fromName(name)
  override def toString: String = s"[$name]"

object Summit:
  def toName(position: Position): SummitName = s"${position.row}-${position.col}"
  private def fromName(name: SummitName): Position =
    name match
      case s"${row}-${col}" => Position(row.toInt, col.toInt)

case class SummitsHolder(allSummits: Seq[Summit]):
  def makeFullDuplexConnexions: SummitsHolder =
    val summitsWithDualConnexions =
      allSummits.map:
        currentSummit =>
          val backlinks = allSummits.filterNot(_ == currentSummit).flatMap:
            otherSummit =>
              otherSummit.nexts.find:
                (summitName, _) => summitName == currentSummit.name
              .map((_, distanceToSummit) => (otherSummit.name, distanceToSummit))
          Summit(currentSummit.name, (currentSummit.nexts ++ backlinks).distinct)
    SummitsHolder(summitsWithDualConnexions)

  private val byName: Map[String, Summit] = allSummits.map(current => current.name -> current).toMap
  private lazy val nextOfArray: Array[List[Int]] = allSummits.toArray.map:
    _.nexts.map(current => byName(current._1)).map(getIndex)
  private lazy val distanceBetweenArray: Array[Array[Int]] =
    allSummits.toArray.map:
      summit => allSummits.toArray.map:
        insideSummit => summit.nexts.find(_._1 == insideSummit.name).fold(Int.MaxValue)(_._2)
  private def getIndex(summit: Summit): Int = allSummits.indexOf(summit)
  private def fromIndex(index: Int): Summit = allSummits(index)

  def byPosition(position: Position): Int = getIndex(byName(Summit.toName(position)))
  def nextOf(index: Int): List[Int] = nextOfArray(index)
  def distanceBetween(first: Int, second: Int): Int = distanceBetweenArray(first)(second)



case class WalkingStatus(summitsToExplore: List[Summit], nextPosition: Option[Position], currentSteps: Int, alreadyExploredPositions: List[Position], alreadyExploredSummits: List[Summit], part2: Boolean = false)(using forest: Forest):
  def goNext = part2 match
      case true => forest.nextPart2
      case false => forest.nextPart1
  def nextPositionsAuthorized(from: Position) = goNext(from).filterNot(alreadyExploredPositions.contains).filterNot(_ == summitsToExplore.head.position)

  def nextPositionIsNotDefined: Boolean = nextPosition.isEmpty
  def nextPositionIsACrossRoad: Boolean = nextPosition.fold(false)(forest.isACrossRoad)

  def nextPositionFromHead: List[Position] = nextPositionsAuthorized(summitsToExplore.head.position)

  def summitFoundIsAKnownOne: Option[Summit] = (summitsToExplore ::: alreadyExploredSummits).find(_.position == nextPosition.get)

  def popHeadAndContinue: WalkingStatus =
    val head :: tail = summitsToExplore : @unchecked
    this.copy(summitsToExplore = tail, nextPosition = None, currentSteps = 0, alreadyExploredSummits = head +: alreadyExploredSummits)

  def continueViaHead(nextPosition: Position): WalkingStatus =
    this.copy(nextPosition = Some(nextPosition), currentSteps = 1)

  def walk: WalkingStatus =
    val goingTo = nextPositionsAuthorized(nextPosition.get).head
    this.copy(nextPosition = Some(goingTo), currentSteps = currentSteps + 1, alreadyExploredPositions = nextPosition.get +: alreadyExploredPositions)

  def passingThroughKnownSummit(knownSummit: Summit): WalkingStatus =
    val head :: tail = summitsToExplore : @unchecked
    val updatedHead = head.copy(nexts = (knownSummit.name, currentSteps) +: head.nexts)
    this.copy(summitsToExplore = updatedHead :: tail, nextPosition = None, currentSteps = 0)

  def passingThroughNewSummit: WalkingStatus =
    val head :: tail = summitsToExplore : @unchecked
    val newSummit = Summit(Summit.toName(nextPosition.get), Nil)
    val updatedCurrentHead = head.copy(nexts = (newSummit.name, currentSteps) +: head.nexts)
    this.copy(summitsToExplore = updatedCurrentHead :: (tail :+ newSummit), nextPosition = None, currentSteps = 0)

  lazy val situation: WalkingStatus.MainSituations =
    summitsToExplore match
      case Nil => NoMoreSummitToExplore
      case head :: tail =>
        nextPosition match
          case None => NextPositionIsNotDefinedYet
          case Some(nexPos) if nextPositionIsACrossRoad => NextPositionIsACrossRoad
          case _ => NextPositionIsAPath

object WalkingStatus:
  enum MainSituations:
    case NoMoreSummitToExplore, NextPositionIsNotDefinedYet, NextPositionIsACrossRoad, NextPositionIsAPath

export WalkingStatus.MainSituations.*

@tailrec
def findNextSummit(walkingStatus: WalkingStatus)(using forest: Forest): List[Summit] =
  walkingStatus.situation match
    case NoMoreSummitToExplore => walkingStatus.alreadyExploredSummits
    case NextPositionIsAPath => findNextSummit(walkingStatus.walk)
    case NextPositionIsNotDefinedYet =>
      val nextStatus = walkingStatus.nextPositionFromHead match
        case Nil => walkingStatus.popHeadAndContinue
        case newNextPosition :: _ => walkingStatus.continueViaHead(newNextPosition)
      findNextSummit(nextStatus)
    case NextPositionIsACrossRoad =>
      val nexStatus = walkingStatus.summitFoundIsAKnownOne match
        case None => walkingStatus.passingThroughNewSummit
        case Some(previouslyFound) => walkingStatus.passingThroughKnownSummit(previouslyFound)
      findNextSummit(nexStatus)

def findSummits(part2: Boolean)(using forest: Forest): List[Summit] =
  val start = Position(0,1)
  val initialSummit = Summit(Summit.toName(start), Nil)
  findNextSummit(WalkingStatus(List(initialSummit), Some(start), 0, Nil, Nil, part2))

case class Forest(places: Array[Array[TypeOfLocation]]):
  private lazy val height = places.length
  private lazy val width = places(0).length
  lazy val startPosition = Position(0, 1)
  lazy val endPosition = Position(height-1, width-2)

  private def isReachable(position: Position): Boolean =
    val Position(row, col) = position
    places.isDefinedAt(row) && places(row).isDefinedAt(col) && places(row)(col) != Tree

  private def isReachableFrom(position: Position, from: Position): Boolean =
    isReachable(position) && {
      val Position(row, col) = position
      (places(row)(col), row - from.row, col - from.col) match
        case (Path, _, _) => true
        case (SlopeUp, rowChange, _) if rowChange < 0 => true
        case (SlopeDown, rowChange, _) if rowChange > 0 => true
        case (SlopeLeft, _, colChange) if colChange < 0 => true
        case (SlopeRight, _, colChange) if colChange > 0 => true
        case _ => false
    }

  def nextPart1(position: Position): List[Position] = position.around.filter(isReachableFrom(_, position))
  def nextPart2(position: Position): List[Position] = position.around.filter(isReachable)

  def isACrossRoad(position: Position): Boolean = position.around.count(isReachable) > 2 || position == endPosition

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
