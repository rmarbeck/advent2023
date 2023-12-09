import scala.io.Source

// Right :-/ result is 54578

@main def hello: Unit =
  println("Launching 1-12-2")
  val bufferedSource = Source.fromFile("./src/main/resources/test2.txt")
  println(s"Result is ${bufferedSource.getLines.map(manageSpecialCasesIFindABitControversial).map(digitalizeProgessively(_, toFindIn)).map(extract).sum}")
  bufferedSource.close
  println("Done")

val toFindIn = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex.map((val1, val2) => (val1, val2 + 1))

def manageSpecialCasesIFindABitControversial(lineContent: String): String =
  lineContent
      .replace("twone", "twoone")
      .replace("threeight", "threeeight")
      .replace("eightwo", "eighttwo")
      .replace("sevenine", "sevennine")
      .replace("nineight", "nineeight")
      .replace("fiveight", "fiveeight")
      .replace("oneight", "oneeight")

def digitalizeProgessively(lineContent: String, toSwap: Seq[(String, Int)]): String =
  def digitalizeBeginingOnly(lineContent: String, index: Int): String =
    val (start, end) = lineContent.splitAt(index)
    digitalize(start, toSwap) + end
  def digitalizeRecursively(toDigitalize: String, currentIndex: Int): String =
    currentIndex match
      case _ if (currentIndex == lineContent.length) => toDigitalize
      case _ => digitalizeRecursively(digitalizeBeginingOnly(toDigitalize, currentIndex + 1), currentIndex + 1)
  digitalizeRecursively(lineContent, 0)

def digitalize(lineContent: String, toSwap: Seq[(String, Int)]): String =
  def doSwap(toReplaceIn: String, vals: (String, Int)): String =
    toReplaceIn.replace(vals._1, vals._2.toString)
  toSwap match
    case Seq(onlyOne) => doSwap(lineContent, onlyOne)
    case _ => digitalize(doSwap(lineContent, toSwap.head), toSwap.tail)

def extract(lineContent: String): Int =
  def first(lineContent: String): Int =
    lineContent.find(_.isDigit).map(_.asDigit.toInt).getOrElse(0)
  first(lineContent)*10+first(lineContent.reverse)
