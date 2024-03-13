object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val summits = findSummits(using Forest.from(inputLines))

    println(summits.filterNot(_.previous.isEmpty).groupBy(_.previous).map((key, value) => key.get.position -> s"${value.map(current => s"${current.position} (${current.distanceToPrevious})").mkString(",")}").mkString("\n"))

    println(summits)

    val result1 = s""
    val result2 = s""

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


case class Summit(position: Position, distanceToPrevious: Int, previous: Option[Summit]):
  override def toString: String = s"[${position.row}, ${position.col}] <- $distanceToPrevious -> ${previous.map(_.position).fold("")(pos => s"[${pos.row}, ${pos.col}]")}"

def walkNext(current: Position, alreadyExplored: List[Position] = Nil)(using forest: Forest) =
  current match
    case Position(0, 1) =>

def findNext(summits: List[Summit], nextPosition: Option[Position], currentSteps: Int, alreadyExploredPositions: List[Position], alreadyExploredSummits: List[Summit])(using forest: Forest): List[Summit] =
  summits match
    case Nil => alreadyExploredSummits
    case head :: tail =>
      nextPosition match
        case None =>
          forest.next(head.position).filterNot(alreadyExploredPositions.contains) match
            case Nil => findNext(tail, None, 0, alreadyExploredPositions, head +: alreadyExploredSummits)
            case newNextPosition :: _ => findNext(summits, Some(newNextPosition), 0, alreadyExploredPositions, alreadyExploredSummits)
        case Some(nextPos) =>
          val newAlreadyExploredPositions = nextPos +: alreadyExploredPositions
          forest.isASummit(nextPos) match
            case true =>
              summits.count(_.position == nextPos) match
                case 0 =>
                  val newSummit = Summit(nextPos, currentSteps, Some(head))
                  val (newSummits, newAlreadyExploredSummits, newNextPosition) =
                    forest.next(head.position).filterNot(newAlreadyExploredPositions.contains) match
                      case Nil =>
                        val summitsWithoutHead = tail :+ newSummit
                        val newNextPosition = forest.next(summitsWithoutHead.head.position).filterNot(newAlreadyExploredPositions.contains).headOption
                        (summitsWithoutHead, head +: alreadyExploredSummits, newNextPosition)
                      case possibleNexts => (summits :+ newSummit, alreadyExploredSummits, possibleNexts.headOption)

                  findNext(newSummits, newNextPosition, 0, newAlreadyExploredPositions, newAlreadyExploredSummits)
                case _ => throw Exception(s"Summit ${nextPosition} already explored, abnormal situation")
            case false => findNext(summits, forest.next(nextPos).filterNot(alreadyExploredPositions.contains).headOption, currentSteps+1, nextPos +: alreadyExploredPositions, alreadyExploredSummits)

def findSummits(using forest: Forest): List[Summit] =
  val start = Position(0,1)
  val initialSummit = Summit(start, 0, None)
  findNext(List(initialSummit), Some(start), 0, Nil, Nil)

case class Forest(places: Array[Array[TypeOfLocation]]):
  lazy val height = places.length
  lazy val width = places(0).length
  def nbOfSlopes: Int = places.flatten.count:
    case SlopeUp | SlopeDown | SlopeLeft | SlopeRight => true
    case _ => false

  private def isReachable(position: Position): Boolean =
    val Position(row, col) = position
    places.isDefinedAt(row) && places(row).isDefinedAt(col) && places(row)(col) != Tree

  def next(position: Position): List[Position] = position.around.filter(isReachable)

  def isASummit(position: Position): Boolean = next(position).length > 2

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
