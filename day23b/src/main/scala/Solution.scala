import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result1 =
      given Grid = Grid.fromInputPart1(inputLines)
      val dimension = summon[Grid].size
      solver(TreeSet((0, Summit(1, 0), Nil)), Set(), Summit(dimension-2, dimension-1)).get.toString


    val result2 =
      given Grid = Grid.fromInputPart2(inputLines)

      val dimension = summon[Grid].size
      solver(TreeSet((0, Summit(1, 0), Nil)), Set(), Summit(dimension - 2, dimension - 1)).get.toString


    (s"$result1", s"$result2")

class Grid(private val arrayOfVals: Array[Array[Char]]):
  lazy val size = arrayOfVals.length
  def next(summit: Summit): Seq[Summit] =
    val Summit(x, y) = summit
    def nextValid(deltas: Seq[(Int, Int)]): Seq[Summit] =
      deltas.map((dX, dY) => (x+dX, y+dY))
        .filter((newX, newY) => arrayOfVals.isDefinedAt(newX) && arrayOfVals(newX).isDefinedAt(newY))
        .filter((newX, newY) => arrayOfVals(newX)(newY) != '#')
        .map((newX, newY) => Summit(newX, newY))

    val atMost = arrayOfVals(x)(y) match
      case '.' =>
        Seq((0, 1), (0, -1), (1, 0), (-1, 0))
      case '>' => Seq((1, 0))
      case '<' => Seq((-1, 0))
      case '^' => Seq((0, -1))
      case 'v' => Seq((0, 1))
      case _ => Seq.empty

    nextValid(atMost)

object Grid:
  private def slopeAsNormalPath(char: Char): Char =
    char match
      case '#' => '#'
      case _ => '.'

  def fromInputPart1(inputLines: Seq[String]): Grid = new Grid(inputLines.map(_.toCharArray).toArray.transpose)
  def fromInputPart2(inputLines: Seq[String]): Grid = new Grid(inputLines.map(_.toCharArray.map(slopeAsNormalPath)).toArray.transpose)

case class Summit(x: Int, y: Int):
  def next(using grid: Grid): Seq[Summit] = grid.next(this)

object Summit:
  given orderingSummit: Ordering[Summit] = Ordering.by(s => (s.x, s.y))
  given orderingSummits: Ordering[List[Summit]] = Ordering.by(_.headOption)

extension (self: List[Summit])
  def containsTwice(summit: Summit) = self.count(_ == summit) >= 2

@tailrec
def solver(toExplore: TreeSet[(Int, Summit, List[Summit])], explored: Set[Summit], toReach: Summit)(using grid: Grid): Option[Int] =
  toExplore match
    case empty if empty.isEmpty => None
    case notEmpty =>
      notEmpty.head match
        case (distance, summit, list) if summit == toReach => Some(distance)
        case (distance, summit, list) =>
          val currentPath = toExplore.head._3
          solver(toExplore.tail ++ summit.next.filterNot(currentPath.contains).map(sum => (distance - 1, sum, sum :: list)), explored + notEmpty.head._2, toReach)