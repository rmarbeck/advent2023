import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given forest: Forest = Forest.from(inputLines)
    val resultPart1 = {
      val summits = findSummits(part2 = false)//.sortBy(-_.nexts.length).distinctBy(_.name)
      given summitsHolder: SummitsHolder = SummitsHolder(summits)

      val start = summitsHolder.byPosition(forest.startPosition)
      val sortedGraph = topologicalSort(start)

      countTopologicallySorted(sortedGraph)
    }

    val resultPart2 = {
      val summits = findSummits(part2 = true)

      val summitsWithBacklinks = summits.map:
        currentSummit =>
          val backlinks = summits.filterNot(_ == currentSummit).flatMap:
            otherSummit =>
              otherSummit.nexts.find:
                (nextDistanceToSummit, _) => nextDistanceToSummit == currentSummit.name
              .map(otherSummitThatContainsCurrentInNext => (otherSummit.name, otherSummitThatContainsCurrentInNext._2))
          Summit(currentSummit.name, (currentSummit.nexts ::: backlinks).distinct)

      given summitsHolder2: SummitsHolder = SummitsHolder(summitsWithBacklinks)
      val start = summitsHolder2.byPosition(forest.startPosition)
      val end = summitsHolder2.byPosition(forest.endPosition)
      val result2 = countDFS(start, end)
      result2.get
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

  override def toString: String = s"$row,$col"

enum TypeOfLocation:
  case Path, Tree, SlopeUp, SlopeDown, SlopeLeft, SlopeRight

export TypeOfLocation.*

type SummitName = String

type DistanceToCrossRoad = (CrossRoad, Int)
type DistanceToSummit = (SummitName, Int)


case class Summit(name: SummitName, nexts: List[DistanceToSummit]):
  lazy val position: Position = Summit.fromName(name)
  override def toString: String = s"[$name]"
  //override def toString: String = s"\n[$name] ${nexts.map(current => s"\n  |\n   -> ${current._1} (${current._2})").mkString}"

object Summit:
  def toName(crossRoad: CrossRoad): SummitName = toName(crossRoad.position)
  def toName(position: Position): SummitName = s"${position.row}-${position.col}"
  def fromName(name: SummitName): Position =
    name match
      case s"${row}-${col}" => Position(row.toInt, col.toInt)

case class SummitsHolder(allSummits: Seq[Summit]):
  val byName: Map[String, Summit] = allSummits.map(current => current.name -> current).toMap
  def byPosition(position: Position): Summit = byName(Summit.toName(position))
  val nextOfCache: collection.mutable.Map[String, List[Summit]] = collection.mutable.Map()
  def nextOf(summit: Summit): List[Summit] =
    nextOfCache.getOrElseUpdate(summit.name, summit.nexts.map(_._1).map(byName))
  val distanceCache: collection.mutable.Map[String, Int] = collection.mutable.Map()
  def distanceBetween(first: Summit, second: Summit): Int =
    distanceCache.get(s"${second.name}${first.name}").getOrElse {
      distanceCache.getOrElseUpdate(s"${first.name}${second.name}", first.nexts.find(current => current._1 == second.name).map(_._2).get)
    }

case class CrossRoad(position: Position, from: List[DistanceToCrossRoad] = Nil)

case class WalkingStatus(summitsToExplore: List[Summit], nextPosition: Option[Position], currentSteps: Int, alreadyExploredPositions: List[Position], alreadyExploredSummits: List[Summit], part2: Boolean = false)(using forest: Forest):
  def goNext = part2 match
      case true => forest.nextPart2
      case false => forest.nextPart1
  def nextPositionsAuthorized(from: Position) = goNext(from).filterNot(alreadyExploredPositions.contains).filterNot(_ == summitsToExplore.head.position)

  def nextPositionIsNotDefined: Boolean = nextPosition.isEmpty
  def nextPositionIsACrossRoad: Boolean = nextPosition.fold(false)(forest.isACrossRoad)

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

@tailrec
def findNextSummit(walkingStatus: WalkingStatus)(using forest: Forest): List[Summit] =
  walkingStatus.summitsToExplore match
    case Nil => walkingStatus.alreadyExploredSummits
    case head :: tail =>
      walkingStatus match
        case currentStatus if currentStatus.nextPositionIsNotDefined =>
          val nextStatus = currentStatus.nextPositionsAuthorized(head.position) match
            case Nil => currentStatus.popHeadAndContinue
            case newNextPosition :: _ => currentStatus.continueViaHead(newNextPosition)
          findNextSummit(nextStatus)
        case currentStatus if currentStatus.nextPositionIsACrossRoad =>
          val nexStatus = (currentStatus.summitsToExplore ::: currentStatus.alreadyExploredSummits).find(_.position == currentStatus.nextPosition.get) match
            case Some(previouslyFound) => currentStatus.passingThroughKnownSummit(previouslyFound)
            case None => currentStatus.passingThroughNewSummit
          findNextSummit(nexStatus)
        case _ => findNextSummit(walkingStatus.walk)

def findSummits(part2: Boolean)(using forest: Forest): List[Summit] =
  val start = Position(0,1)
  val initialSummit = Summit(Summit.toName(start), Nil)
  findNextSummit(WalkingStatus(List(initialSummit), Some(start), 0, Nil, Nil, part2))



case class Forest(places: Array[Array[TypeOfLocation]]):
  lazy val height = places.length
  lazy val width = places(0).length
  lazy val startPosition = Position(0, 1)
  lazy val endPosition = Position(height-1, width-2)
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
