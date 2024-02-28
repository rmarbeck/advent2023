object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val valleys = inputLines.map:
      case "" => ";"
      case value => value
    .mkString(",").split(";").map(_.split(",").filterNot(_.isEmpty)).map(Valley(_))

    valleys.foreach(println)

    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

case class Valley(input: Array[String]):
  val lineValues =
    input.map:
      case line => line.foldLeft(0l):
        case (acc, '#') => acc * 2 + 1
        case (acc, _) => acc * 2

  override def toString: String = s"${lineValues.mkString(",")}"