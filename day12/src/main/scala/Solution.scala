import collection.parallel.CollectionConverters.seqIsParallelizable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val holders = inputLines.map:
      case s"$springs $groups" => SpringHolder(springs, groups.split(",").toList.map(_.toInt))

    val holdersPart2 = holders.map(_.asPart2)

    val result1 = s"${holders.map(_.arrangements).sum}"
    val result2 = s"${holdersPart2.par.map(_.arrangements).sum}"

    (s"${result1}", s"${result2}")

end Solution

case class WorkInProgessSpringHolder(holder: SpringHolder, workedOnReverse: String)

case class SpringHolder(rawValues: String, groups: List[Int]):
  def asPart2: SpringHolder =
    val newRawValues = (1 until 5).foldLeft(rawValues):
      case (acc, _) => acc + "?" + rawValues

    val newGroups = (1 until 5).foldLeft(groups):
      case (acc, _) => acc ::: groups

    SpringHolder(newRawValues, newGroups)

  lazy val arrangements: Long =
    def compute(holders: List[WorkInProgessSpringHolder], validArrangements: Int): Int =
      holders match
        case Nil => validArrangements
        case head :: tail =>
          val currentHeadValues = head.holder.rawValues
          currentHeadValues.headOption match
            case Some('.') =>
              head.holder.groups.headOption match
                case Some(value) if value == 0 =>
                  val newInnerSpringHolder = head.holder.copy(rawValues = currentHeadValues.tail, groups = head.holder.groups.tail)
                  val newWorkingHolder = head.copy(holder = newInnerSpringHolder, workedOnReverse = "." + head.workedOnReverse)
                  compute(newWorkingHolder +: tail, validArrangements)
                case Some(value) if value != 0 && head.workedOnReverse.headOption.contains('#') => compute(tail, validArrangements)
                case _ =>
                  val newInnerSpringHolder = head.holder.copy(rawValues = currentHeadValues.tail)
                  val newWorkingHolder = head.copy(holder = newInnerSpringHolder, workedOnReverse = "." + head.workedOnReverse)
                  compute(newWorkingHolder +: tail, validArrangements)
            case Some('#') =>
              head.holder.groups.headOption match
                case Some(value) if value != 0 =>
                  val newInnerSpringHolder = head.holder.copy(rawValues = currentHeadValues.tail, groups = (value - 1) +: head.holder.groups.tail)
                  val newWorkingHolder = head.copy(holder = newInnerSpringHolder, workedOnReverse = "#" + head.workedOnReverse)
                  compute(newWorkingHolder +: tail, validArrangements)
                case _ => compute(tail, validArrangements)
            case Some('?') =>
              val headWithDot = head.holder.copy(rawValues = "." + currentHeadValues.tail)
              val headWithHash = head.holder.copy(rawValues = "#" + currentHeadValues.tail)
              compute((head.copy(holder = headWithDot) :: head.copy(holder = headWithHash) :: Nil) ::: tail, validArrangements)
            case None =>
              head.holder.groups match
                case Nil | List(0) => compute(tail, validArrangements + 1)
                case _ => compute(tail, validArrangements)
            case _ => throw Exception("Should not happen")

    compute(List(WorkInProgessSpringHolder(this, "")), 0)