import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val data = inputLines.map:
      case s"$data:$values" => values.split(" ").withFilter(_.nonEmpty).map(_.toLong)

    val Seq(result1, result2) = Seq(Race.fromDataPart1, Race.fromDataPart2).map(_.apply(data).map(_.findWaysToBeatTheRecord).product)

    (s"$result1", s"$result2")

end Solution

case class Race(time: Long, record: Long):
  @tailrec
  private def nbIntBetweenStrict(first: Double, second: Double): Int =
    def exactSolutionToRemove(rawSol: Double, roundedSol: Double): Int = if rawSol == roundedSol then 1 else 0

    if first > second then
      val (firstRounded, secondRounded) = (math.floor(first), math.ceil(second))
      val raw = (firstRounded - secondRounded + 1).toInt
      raw - exactSolutionToRemove(first, firstRounded) - exactSolutionToRemove(second, secondRounded)
    else
      nbIntBetweenStrict(second, first)

  def findWaysToBeatTheRecord: Int =
    val (a, b, c) = (-1, time.toDouble, -record)
    val deltaSquareRoot: Double = math.sqrt( b * b - 4 * a * c )
    val solution1 = (- b - deltaSquareRoot) / ( 2 * a )
    val solution2 = (- b + deltaSquareRoot) / ( 2 * a )

    nbIntBetweenStrict(solution1, solution2)

object Race:
  def fromDataPart1(data: Seq[Array[Long]]): Seq[Race] =
    data.head.zip(data.last).map((time, record) => Race(time, record)).toIndexedSeq

  def fromDataPart2(data: Seq[Array[Long]]): Seq[Race] =
    val Seq(time, record) = data.map(_.mkString.toLong)
    Seq(Race(time, record))