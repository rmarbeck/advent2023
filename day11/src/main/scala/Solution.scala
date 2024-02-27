import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val universe = Universe:
      inputLines.zipWithIndex.flatMap:
        case (line, row) =>
          line.zipWithIndex.flatMap:
            case ('#', col) => Some(Galaxy(row, col))
            case _ => None

    val result1 = s"${universe.minimalDistances()}"
    val result2 = s"${universe.minimalDistances(1000000)}"

    (s"${result1}", s"${result2}")

end Solution

case class Galaxy(row: Int, col: Int):
  def taxiDistanceWith(other: Galaxy): Long = math.abs(row - other.row) + math.abs(col - other.col)

case class Universe(galaxies: Seq[Galaxy]):
  private lazy val rows = galaxies.map(_.row)
  private lazy val cols = galaxies.map(_.col)
  private lazy val height = rows.max + 1
  private lazy val width = cols.max + 1

  private lazy val emptyRows = scala.collection.immutable.TreeSet((0 until height) diff rows:_*)
  private lazy val emptyCols = scala.collection.immutable.TreeSet((0 until width) diff cols:_*)

  private def emptySpaceBetween(firstGalaxy: Galaxy, secondGalaxy: Galaxy): Long =
    def nbMatching(rawSet: Set[Int], limit1: Int, limit2: Int): Int =
      limit1 <= limit2 match
        case true =>
          rawSet.count:
            case value if value > limit1 && value < limit2 => true
            case _ => false
        case false => nbMatching(rawSet, limit2, limit1)

    val nbOfEmptyRowsIncluded = nbMatching(emptyRows, firstGalaxy.row, secondGalaxy.row)
    val nbOfEmptyColsIncluded = nbMatching(emptyCols, firstGalaxy.col, secondGalaxy.col)

    nbOfEmptyRowsIncluded + nbOfEmptyColsIncluded

  def minimalDistances(expansion: Long = 2): Long =
    val expansionFactor = expansion - 1
    @tailrec
    def distances(galaxies: List[Galaxy], current: Long = 0): Long =
      galaxies match
        case head :: Nil => current
        case head :: tail =>
          val subDistances = tail.foldLeft(0l):
            case (acc, current) => acc + current.taxiDistanceWith(head) + (emptySpaceBetween(current, head) * expansionFactor)
          distances(tail, current + subDistances)
        case _ => throw Exception("Not supported")

    distances(galaxies.toList)