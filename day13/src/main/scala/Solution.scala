import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val valleys =
      inputLines.foldLeft(List(Nil): List[List[String]]):
        case (acc, "") => Nil +: acc
        case (acc, newLine) => (acc.head :+ newLine) +: acc.tail
      .map(_.toArray)
    .map(Valley(_))

    val result1 = s"${valleys.map(_.countPart1).sum}"
    val result2 = s"${valleys.map(_.countPart2).sum}"

    (s"${result1}", s"${result2}")

end Solution


type Matcher = (Array[Long], Array[Long]) => Boolean

case class Valley(input: Array[String]):
  def toPowerOf2LongArray(fromArray: Array[Array[Char]]): Array[Long] =
    fromArray.map:
      line => line.foldLeft(0l):
        case (acc, '#') => acc * 2 + 2
        case (acc, _) => acc * 2

  private lazy val horizontalValues: Array[Long] = toPowerOf2LongArray(input.map(_.toCharArray))

  private lazy val verticalValues: Array[Long] = toPowerOf2LongArray(input.map(_.toCharArray).transpose)

  private val part1Matcher: Matcher = (first, second) => first.zip(second).forall(_ == _)

  private val part2Matcher: Matcher = (first, second) =>
      first.zip(second).filterNot(_ == _) match
        case Array((value1, value2)) if isAPowerOfTwo(value1 - value2) => true
        case _ => false

  lazy val countPart1: Int = count(using part1Matcher)

  lazy val countPart2: Int = count(using part2Matcher)

  def count(using Matcher): Int = find(horizontalValues).map(_*100) orElse find(verticalValues) getOrElse 0

  @tailrec
  private def find(toAnalyse: Array[Long], analysed: Array[Long] = Array(), counter: Int = 0)(using matcher: Matcher): Option[Int] =
    toAnalyse.isEmpty match
      case true => None
      case false =>
        analysed.isEmpty match
          case true => find(toAnalyse.tail, Array(toAnalyse.head), 1)
          case false =>
            matcher(toAnalyse, analysed) match
              case true => Some(counter)
              case false => find(toAnalyse.tail, toAnalyse.head +: analysed, counter + 1)

  private def isAPowerOfTwo(value: Long): Boolean =
    value > 0 match
      case false => isAPowerOfTwo(-value)
      case true =>
        value match
          case 2 => true
          case current if current % 2 != 0 => false
          case current => isAPowerOfTwo(current / 2)