object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val data = inputLines.map:
      case s"$data:$values" => values.trim.split(" ").filterNot(_.isEmpty).map(_.toLong)

    val resultPart1 = Race.fromDataPart1(data).map(_.findWaysToBeatTheRecord).product
    val resultPart2 = Race.fromDataPart2(data).map(_.findWaysToBeatTheRecord).product

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Race(time: Long, record: Long):
  private def nbInt(first: Double, second: Double): Int =
    def removeExactSolutions(rawSol: Double, roundedSol: Double): Int =
      rawSol == roundedSol match
        case true => 1
        case false => 0
    first > second match
      case true =>
        val (firstRounded, secondRounded) = (math.floor(first), math.ceil(second))
        val raw = (firstRounded - secondRounded + 1).toInt
        raw - removeExactSolutions(first, firstRounded) - removeExactSolutions(second, secondRounded)
      case false => nbInt(second, first)

  def findWaysToBeatTheRecord: Int =
    val (a, b, c) = (-1, time, -record)
    val delta: Double = b.toDouble * b - 4 * a *c
    val solution1 = (- b - math.sqrt(delta)) / (2 * a)
    val solution2 = (- b + math.sqrt(delta)) / (2 * a)
    nbInt(solution1, solution2)

object Race:
  def fromDataPart1(data: Seq[Array[Long]]): Seq[Race] =
    data.head.zipWithIndex.map((time, index) => Race(time, data(1)(index))).toIndexedSeq

  def fromDataPart2(data: Seq[Array[Long]]): Seq[Race] =
    val Seq(time, record) = data.map:
      case values => values.foldLeft(""):
        case (acc, newLong) => acc + newLong.toString
    Seq(Race(time.toLong, record.toLong))