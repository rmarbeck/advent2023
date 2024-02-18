object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val data = inputLines.map:
      case s"$data:$values" => values.split(" ").filterNot(_.isEmpty).map(_.toLong)

    val Seq(resultPart1, resultPart2) = Seq(Race.fromDataPart1, Race.fromDataPart2).map(_.apply(data).map(_.findWaysToBeatTheRecord).product)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Race(time: Long, record: Long):
  private def nbIntBetween(first: Double, second: Double): Int =
    def removeExactSolutions(rawSol: Double, roundedSol: Double): Int =
      rawSol == roundedSol match
        case true => 1
        case false => 0
    first > second match
      case true =>
        val (firstRounded, secondRounded) = (math.floor(first), math.ceil(second))
        val raw = (firstRounded - secondRounded + 1).toInt
        raw - removeExactSolutions(first, firstRounded) - removeExactSolutions(second, secondRounded)
      case false => nbIntBetween(second, first)

  def findWaysToBeatTheRecord: Int =
    val (a, b, c) = (-1, time.toDouble, -record)
    val deltaSquareRoot: Double = math.sqrt( b * b - 4 * a *c )
    val solution1 = (- b - deltaSquareRoot) / (2 * a)
    val solution2 = (- b + deltaSquareRoot) / (2 * a)
    nbIntBetween(solution1, solution2)

object Race:
  def fromDataPart1(data: Seq[Array[Long]]): Seq[Race] =
    data.head.zip(data.last).map((time, record) => Race(time, record)).toIndexedSeq

  def fromDataPart2(data: Seq[Array[Long]]): Seq[Race] =
    val Seq(time, record) = data.map(_.mkString.toLong)
    Seq(Race(time, record))