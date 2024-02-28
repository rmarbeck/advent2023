import collection.parallel.CollectionConverters.seqIsParallelizable
import scala.annotation.tailrec
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val holders = inputLines.map:
      case s"$springs $groups" => SpringHolder(springs, groups.split(",").toList.map(_.toInt))

    /*val holdersPart2 = holders.foreach:
      case holder =>
        holder.part2Stats match
          case vals @ (_, dual, dot, hash, three, guessed) if three < guessed => println(s" ---- !! ----- $holder => ${vals}")
          case vals @ (_, dual, dot, hash, three, guessed) if three != guessed => println(s"$holder => ${vals}")
          case _ => ()*/

    val holdersPart2 = holders.map(_.asPart2)

    val result1 = s"${holders.map(_.arrangements2).sum}"
    val result2 = s"${holdersPart2.par.map(_.arrangements2).sum}"

    (s"${result1}", s"${result2}")

end Solution

case class WorkInProgessSpringHolder(holder: SpringHolder, workedOnReverse: String):
  def removeLeadingChar =
    val updatedHolder = holder.copy(rawValues = holder.rawValues.tail)
    this.copy(holder = updatedHolder)
  def decrementLeadingGroup =
    val updatedHolder = holder.copy(groups = (holder.groups.head - 1) +: holder.groups.tail)
    this.copy(holder = updatedHolder)
  def removeLeadingGroup =
    val updatedHolder = holder.copy(groups = holder.groups.tail)
    this.copy(holder = updatedHolder)
  def replaceLeadingChar(char: Char) =
    val updatedHolder = holder.copy(rawValues = s"${char}${holder.rawValues.tail}")
    this.copy(holder = updatedHolder)
  def hasWorkedOn(char: Char) =
    this.copy(workedOnReverse = s"${char}${this.workedOnReverse}")

case class SpringHolder(rawValues: String, groups: List[Int]):
  def asPart2: SpringHolder =
    val newRawValues = (1 until 5).foldLeft(rawValues):
      case (acc, _) => acc + "?" + rawValues

    val newGroups = (1 until 5).foldLeft(groups):
      case (acc, _) => acc ::: groups

    SpringHolder(newRawValues, newGroups)

  def part2Stats: (Long, Long, Long, Long, Long, Long) =
    val single = this.arrangements
    val dual = this.copy(rawValues = rawValues + "?" + rawValues, groups = groups ::: groups).arrangements
    val dot = this.copy(rawValues = rawValues + "#", groups = groups).arrangements
    val dash = this.copy(rawValues = rawValues + "#" + rawValues, groups = groups ::: groups).arrangements
    val three = this.copy(rawValues = rawValues + "?" + rawValues + "?" + rawValues, groups = groups ::: groups ::: groups).arrangements

    (single, dual, dot, dash, three, single * math.pow(dual/single, 2).toLong)

  lazy val arrangements: Long = computeArrangements(List(WorkInProgessSpringHolder(this, "")), 0)
  lazy val arrangements2: Long = computeArrangements2Cached(rawValues.toList, groups, 0)

@tailrec
def computeArrangements(holders: List[WorkInProgessSpringHolder], validArrangements: Int): Int =
  holders match
    case Nil => validArrangements
    case head :: tail =>
      val currentValuesFromHead = head.holder.rawValues
      val currentGroupsFromHead = head.holder.groups
      currentValuesFromHead.headOption match
        case Some('.') =>
          currentGroupsFromHead.headOption match
            case Some(0) =>
              val updatedCurrentHead = head.removeLeadingChar.removeLeadingGroup.hasWorkedOn('.')
              computeArrangements(updatedCurrentHead +: tail, validArrangements)
            case Some(firstValueInGroup) if firstValueInGroup != 0  && head.workedOnReverse.headOption.contains('#') =>
              computeArrangements(tail, validArrangements)
            case _ =>
              val updatedCurrentHead = head.removeLeadingChar.hasWorkedOn('.')
              computeArrangements(updatedCurrentHead +: tail, validArrangements)
        case Some('#') =>
          currentGroupsFromHead.headOption match
            case Some(firstValueInGroup) if firstValueInGroup != 0 =>
              val updatedCurrentHead = head.decrementLeadingGroup.removeLeadingChar.hasWorkedOn('#')
              computeArrangements(updatedCurrentHead +: tail, validArrangements)
            case _ =>
              computeArrangements(tail, validArrangements)
        case Some('?') =>
          val updatedHeadAsList = List(head.replaceLeadingChar('.'), head.replaceLeadingChar('#'))
          computeArrangements(updatedHeadAsList ::: tail, validArrangements)
        case None =>
          currentGroupsFromHead match
            case Nil | List(0) => computeArrangements(tail, validArrangements + 1)
            case _ => computeArrangements(tail, validArrangements)
        case _ => throw Exception("Should not happen")

val cache : mutable.Map[(List[Char], List[Int], Int), Long] = mutable.HashMap[(List[Char], List[Int], Int), Long]()
def computeArrangements2Cached(springs: List[Char], groups: List[Int], currentBroken: Int): Long =
  cache.getOrElseUpdate((springs, groups, currentBroken), computeArrangements2(springs, groups, currentBroken))

def computeArrangements2(springs: List[Char], groups: List[Int], currentBroken: Int): Long =
  springs match
    case Nil =>
      groups match
        case Nil => 1
        case List(value) if value == currentBroken => 1
        case _ => 0
    case head :: tail =>
      head match
        case '.' if currentBroken == 0 => computeArrangements2Cached(tail, groups, 0)
        case '.' if groups.headOption.exists(_ == currentBroken) => computeArrangements2Cached(tail, groups.drop(1), 0)
        case '#' if groups.headOption.isDefined => computeArrangements2Cached(tail, groups, currentBroken + 1)
        case '?' => computeArrangements2Cached('.' +: tail, groups, currentBroken) + computeArrangements2Cached('#' +: tail, groups, currentBroken)
        case _ => 0

