object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val digits = (1 to 9).map(_.toString)
    val literals = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

    val digitsAndLiterals = digits ++ literals

    val (resultPart1, resultPart2) = inputLines.foldLeft((0, 0)):
      (acc, currentLine) => (acc._1 + extract(currentLine, digits), acc._2 + extract(currentLine, digitsAndLiterals))

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"$result1", s"$result2")

end Solution

case class Statistics(indexFromStart: Int, indexFromEnd: Int, position: Int):
  lazy val digitFromPosition: Int = (position % 9) + 1

def extract(toLookIn: String, sortedDigits: Seq[String]): Int =
  val indexes = sortedDigits.zipWithIndex.flatMap: (currentDigit, index) =>
    toLookIn.indexOf(currentDigit) match
      case -1 => None
      case value => Some(Statistics(value, toLookIn.lastIndexOf(currentDigit), index))

  val fromStart = indexes.minBy(_.indexFromStart).digitFromPosition
  val fromEnd = indexes.maxBy(_.indexFromEnd).digitFromPosition
  fromStart * 10 + fromEnd