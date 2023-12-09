import scala.io.Source

@main def hello: Unit =
  println("Launching 1-12-1")
  val bufferedSource = Source.fromFile("./src/main/resources/test2.txt")
  println(s"Result is ${bufferedSource.getLines.map(extract).sum}")
  bufferedSource.close
  println("Done") 

def extract(lineContent: String): Int =
  def first(lineContent: String): Int =
    lineContent.find(_.isDigit).map(_.asDigit).getOrElse(0)
  first(lineContent)*10+first(lineContent.reverse)

