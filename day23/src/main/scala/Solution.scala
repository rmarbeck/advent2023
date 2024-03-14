object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val resultPart1 = {
      given forest: Forest = Forest.from(inputLines)

      val crossRoads = findCrossRoads

      val summits = crossRoads.map:
        currentCrossRoad =>
          val nextSummits =
            crossRoads.flatMap:
              nextCrossRoad =>
                nextCrossRoad.from.find(_._1.position == currentCrossRoad.position).map:
                  (_, distance) => DistanceToSummit(Summit.toName(nextCrossRoad), distance)

          Summit(Summit.toName(currentCrossRoad), nextSummits)

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
      given forest: Forest = Forest.from(inputLines.map(_.replaceAll("[\\^<>v]+", ".")))

      val crossRoads = findCrossRoads

      val summits = crossRoads.map:
        currentCrossRoad =>
          val nextSummits =
            crossRoads.flatMap:
              nextCrossRoad =>
                nextCrossRoad.from.find(_._1.position == currentCrossRoad.position).map:
                  (_, distance) => DistanceToSummit(Summit.toName(nextCrossRoad), distance)

          Summit(Summit.toName(currentCrossRoad), nextSummits)

      val summitsWithBacklinks = summits.map:
        currentSummit =>
          val backlinks = summits.filterNot(_ == currentSummit).flatMap:
            otherSummit =>
              otherSummit.nexts.find:
                nextDistanceToSummit => nextDistanceToSummit.name == currentSummit.name
              .map(otherSummitThatContainsCurrentInNext => DistanceToSummit(otherSummit.name, otherSummitThatContainsCurrentInNext._2))
          Summit(currentSummit.name, (currentSummit.nexts ::: backlinks).distinct)

      given summitsHolder2: SummitsHolder = SummitsHolder(summitsWithBacklinks)
      val endName = Summit.toName(Position(forest.height-1, forest.width-2))
      val result2 = countPart2(summitsHolder2.byName("0-1"), Nil, summitsHolder2.byName(endName))
      result2.flatMap(_._1).get
      /*val result2 = topologicalSort(summitsHolder2.byName("0-1")).values
      val resultWithData2 = result2.zipWithIndex.map:
        case (currentSummit, 0) =>
          WithData[Summit](currentSummit, 0)
        case (currentSummit, _) =>
          WithData[Summit](currentSummit)
      println(count(resultWithData2))*/
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

case class DistanceToSummit(name: SummitName, distance: Int):
  lazy val sorter: (Int, Int) = (distance, Summit.fromName(name).sorter)

case class Summit(name: SummitName, nexts: List[DistanceToSummit]):
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

def findNext(crossRoads: List[CrossRoad], nextPosition: Option[Position], currentSteps: Int, alreadyExploredPositions: List[Position], alreadyExploredCrossRoads: List[CrossRoad])(using forest: Forest): List[CrossRoad] =
  def nextPositionsAuthorized(from: Position) =
    forest.next(from).filterNot(alreadyExploredPositions.contains).filterNot(_ == crossRoads.head.position)
  crossRoads match
    case Nil => alreadyExploredCrossRoads
    case head :: tail =>
      nextPosition match
        case None =>
          nextPositionsAuthorized(head.position) match
            case Nil => findNext(tail, None, 1, alreadyExploredPositions, head +: alreadyExploredCrossRoads)
            case newNextPosition :: _ => findNext(crossRoads, Some(newNextPosition), 0, alreadyExploredPositions, alreadyExploredCrossRoads)
        case Some(nextPos) =>
          forest.isACrossRoad(nextPos) match
            case true =>
              crossRoads.find(_.position == nextPos) match
                case Some(previouslyFound) =>
                  val newCrossRoad = previouslyFound.copy(from = (head, currentSteps) +: previouslyFound.from)
                  val (newCrossRoads, newAlreadyExploredCrossRoads, newNextPosition) =
                    nextPositionsAuthorized(head.position) match
                      case Nil =>
                        val crossRoadsWithoutHead = tail.filterNot(_ == previouslyFound) :+ newCrossRoad
                        val newNextPosition = nextPositionsAuthorized(crossRoadsWithoutHead.head.position).headOption
                        (crossRoadsWithoutHead, head +: alreadyExploredCrossRoads, newNextPosition)
                      case possibleNexts => (crossRoads.filterNot(_ == previouslyFound) :+ newCrossRoad, alreadyExploredCrossRoads, possibleNexts.headOption)

                  findNext(newCrossRoads, newNextPosition, 1, alreadyExploredPositions, newAlreadyExploredCrossRoads)

                case None =>
                  val newCrossRoad = CrossRoad(nextPos, List((head, currentSteps)))
                  val (newCrossRoads, newAlreadyExploredCrossRoads, newNextPosition) =
                    nextPositionsAuthorized(head.position) match
                      case Nil =>
                        val crossRoadsWithoutHead = tail :+ newCrossRoad
                        val newNextPosition = nextPositionsAuthorized(crossRoadsWithoutHead.head.position).headOption
                        (crossRoadsWithoutHead, head +: alreadyExploredCrossRoads, newNextPosition)
                      case possibleNexts => (crossRoads :+ newCrossRoad, alreadyExploredCrossRoads, possibleNexts.headOption)

                  findNext(newCrossRoads, newNextPosition, 1, alreadyExploredPositions, newAlreadyExploredCrossRoads)
            case false => findNext(crossRoads, nextPositionsAuthorized(nextPos).headOption, currentSteps+1, nextPos +: alreadyExploredPositions, alreadyExploredCrossRoads)

def findCrossRoads(using forest: Forest): List[CrossRoad] =
  val start = Position(0,1)
  val initialCrossRoad = CrossRoad(start)
  findNext(List(initialCrossRoad), Some(start), 0, Nil, Nil)

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

  def next(position: Position): List[Position] = position.around.filter(isReachableFrom(_, position))

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
