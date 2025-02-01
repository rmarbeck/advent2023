import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

val expansionPart2 = 1_000_000

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val universe = Universe:
      inputLines.zipWithIndex.flatMap:
        case (line, row) =>
          line.zipWithIndex.collect:
            case ('#', col) => Galaxy(row, col)

    val result1 = s"${universe.minimalDistances()}"
    val result2 = s"${universe.minimalDistances(expansionPart2)}"

    (s"$result1", s"$result2")

case class Galaxy(row: Int, col: Int):
  def taxiDistanceWith(other: Galaxy): Long = math.abs(row - other.row) + math.abs(col - other.col)

case class Universe(galaxies: Seq[Galaxy]):
  private lazy val (rows, cols) = galaxies.map(gal => (gal.row, gal.col)).unzip
  private lazy val height = rows.max + 1
  private lazy val width = cols.max + 1

  private lazy val emptyRows = ((0 until height) diff rows).toSet
  private lazy val emptyCols = ((0 until width) diff cols).toSet

  private def emptySpaceBetween(firstGalaxy: Galaxy, secondGalaxy: Galaxy): Long =
    @tailrec
    def nbMatching(rawSet: Set[Int], limit1: Int, limit2: Int): Int =
      if limit1 <= limit2 then
        rawSet.count:
          case value if value > limit1 && value < limit2 => true
          case _ => false
      else
        nbMatching(rawSet, limit2, limit1)

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
          val subDistances = tail.foldLeft(0L):
            case (acc, current) => acc + current.taxiDistanceWith(head) + (emptySpaceBetween(current, head) * expansionFactor)
          distances(tail, current + subDistances)
        case _ => throw Exception("Not supported")

    distances(galaxies.toList)