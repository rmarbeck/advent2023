import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val digits = (1 to 9).map(_.toString)
    val literals = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

    val digitsAndLiterals = digits ++ literals
    val digitsAndLiteralsR2L = digits ++ literals.map(_.reverse)

    val (resultPart1, resultPart2) = inputLines.foldLeft((0, 0)):
      (acc, currentLine) => (acc._1 + extractMirror(currentLine, digits, digits), acc._2 + extractMirror(currentLine, digitsAndLiterals, digitsAndLiteralsR2L))

    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

def extractMirror(inLine: String, fromListL2R: Seq[String], fromListR2L: Seq[String]): Int =
  val fromStart = findFirstFrom(inLine, fromListL2R)
  val fromEnd = findFirstFrom(inLine.reverse, fromListR2L)
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
          inList.view.zipWithIndex.find:
            case (currentStringToFind, index) => workingOn.indexOf(currentStringToFind) != -1
        resultForCurrent match
          case None => findInPartialString(upToChar + 1)
          case Some(_, indexFound) => indexFound % 9 + 1

  findInPartialString()