import scala.annotation.tailrec

type Matcher = (Array[Long], Array[Long]) => Boolean
val part1Matcher: Matcher = (first, second) => first zip second forall (_ == _)

val part2Matcher: Matcher =
  def isAPowerOfTwo(value: Long): Boolean =
    value match
      case 0 => false
      case negative if negative < 0 => isAPowerOfTwo(-negative)
      case positive => (positive & (positive - 1)) == 0

  (first, second) =>
    first zip second filterNot (_ == _) match
      case Array((value1, value2)) if isAPowerOfTwo(value1 - value2) => true
      case _ => false

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val valleys =
      inputLines.foldLeft(List(Nil): List[List[String]]):
        case (acc, "") => Nil +: acc
        case (acc, notEmptyLine) => (acc.head :+ notEmptyLine) +: acc.tail
      .map(_.toArray)
    .map(Valley.apply)

    val result1 = s"${valleys.map(_.countPart1).sum}"
    val result2 = s"${valleys.map(_.countPart2).sum}"

    (s"$result1", s"$result2")

case class Valley(input: Array[String]):
  private val inputCharArray = input.map(_.toCharArray)
  private inline def toPowerOf2LongArray(fromArray: Array[Array[Char]]): Array[Long] =
    fromArray.map:
      _.foldLeft(0L):
        case (acc, '#') => acc * 2 + 2
        case (acc, _) => acc * 2

  private lazy val horizontalValues: Array[Long] = toPowerOf2LongArray(inputCharArray)

  private lazy val verticalValues: Array[Long] = toPowerOf2LongArray(inputCharArray.transpose)

  lazy val countPart1: Int = count(using part1Matcher)

  lazy val countPart2: Int = count(using part2Matcher)

  private inline def count(using Matcher): Int = find(horizontalValues).map(_*100) orElse find(verticalValues) getOrElse 0

  @tailrec
  private def find(toAnalyse: Array[Long], analysed: Array[Long] = Array(), counter: Int = 0)(using matcher: Matcher): Option[Int] =
    if toAnalyse.isEmpty then
      None
    else if analysed.isEmpty then
      find(toAnalyse.tail, Array(toAnalyse.head), 1)
    else
      matcher(toAnalyse, analysed) match
        case true => Some(counter)
        case false => find(toAnalyse.tail, toAnalyse.head +: analysed, counter + 1)