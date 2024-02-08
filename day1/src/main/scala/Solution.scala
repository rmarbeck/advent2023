import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val digits = (1 to 9).map(_.toString)
    val literals = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

    val (resultPart1, resultPart2) = inputLines.map:
      case currentLine => (extractMirror(currentLine, digits), extractMirror(currentLine, digits ++ literals))
    .unzip

    val result1 = s"${resultPart1.sum}"
    val result2 = s"${resultPart2.sum}"

    (s"${result1}", s"${result2}")

end Solution

def extractMirror(inLine: String, fromList: Seq[String]): Int =
  val fromStart = findFirstFrom(inLine, fromList)
  val fromEnd = findFirstFrom(inLine.reverse, fromList.map(_.reverse))
  fromStart * 10 + fromEnd

def findFirstFrom(inputString: String, inList: Seq[String]): Int =
  val sizeOfInput = inputString.size
  @tailrec
  def findInPartialString(upToChar: Int = 0): Int =
    upToChar match
      case value if value == sizeOfInput + 1 => throw Exception("Unable to find any number")
      case value =>
        val workingOn = inputString.take(value)
        val resultForCurrent =
          inList.zipWithIndex.find:
            case (currentStringToFind, index) => workingOn.indexOf(currentStringToFind) != -1
        resultForCurrent match
          case None => findInPartialString(upToChar + 1)
          case Some(_, indexFound) => indexFound % 9 + 1

  findInPartialString()