import collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val holdersPart1 = inputLines.map:
      case s"$springs $groups" => SpringHolder(springs, groups.split(",").toList.map(_.toInt))

    val holdersPart2 = holdersPart1.map(_.asPart2)

    val result1 = s"${holdersPart1.map(_.arrangements).sum}"
    val result2 = s"${holdersPart2.par.map(_.arrangements).sum}"

    (s"${result1}", s"${result2}")
end Solution

case class SpringHolder(rawValues: String, groups: List[Int]):
  def asPart2: SpringHolder =
    val (newRawValues, newGroups) = (1 until 5).foldLeft((rawValues, groups)):
      case (acc, _) => (s"${acc(0)}?${rawValues}", acc(1) ::: groups)

    SpringHolder(newRawValues, newGroups)

  lazy val arrangements: Long = computeArrangements(rawValues.toList, groups, 0)


def computeArrangements(springs: List[Char], groups: List[Int], currentBroken: Int): Long =
  val cache = mutable.Map.empty[(List[Char], List[Int], Int), Long]

  def computeArrangementsCached(springs: List[Char], groups: List[Int], currentBroken: Int): Long =
    cache.getOrElseUpdate((springs, groups, currentBroken), computeArrangementsUncached(springs, groups, currentBroken))

  def computeArrangementsUncached(springs: List[Char], groups: List[Int], currentBroken: Int): Long =
    springs match
      case Nil =>
        groups match
          case Nil => 1
          case List(value) if value == currentBroken => 1
          case _ => 0
      case head :: tail =>
        head match
          case '.' if currentBroken == 0 => computeArrangementsCached(tail.dropWhile(_ == '.'), groups, 0)
          case '.' if groups.headOption.contains(currentBroken) => computeArrangementsCached(tail.dropWhile(_ == '.'), groups.drop(1), 0)
          case '#' if groups.nonEmpty =>
            tail.indexWhere(_ != '#') match
              case -1 => computeArrangementsCached(Nil, groups, currentBroken + tail.size + 1)
              case value => computeArrangementsCached(tail.drop(value), groups, currentBroken + value + 1)
          case '?' => computeArrangementsCached('.' +: tail, groups, currentBroken) + computeArrangementsCached('#' +: tail, groups, currentBroken)
          case _ => 0

  computeArrangementsUncached(springs, groups, currentBroken)