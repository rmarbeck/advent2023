import scala.io.Source
import scala.math._

class Line(inputLine: String):
  def extractColor(bags: Array[String], color: String): Int =
    bags.flatMap(current => current.split(',').filter(_.contains(color))).map(_.replace(" "+color, "").trim.toInt).max
  val values = inputLine.split(':')(1)
  val bags = values.split(';')
  val blue = extractColor(bags, "blue")
  val red = extractColor(bags, "red")
  val green = extractColor(bags, "green")

  def game = inputLine.split(':').apply(0).replace("Game ","").toInt
  def blues = blue
  def reds = red
  def greens = green
  def power = blue * red * green
  def passesTest(blueMax: Int, redMax: Int, greenMax: Int) =
    blue <= blueMax && red <= redMax && green <= greenMax

@main def hello: Unit =
  println("Launching 2-12")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val result = bufferedSource.getLines().map(Line(_)).toSeq
  println(s"1 : ${result.filter(_.passesTest(14, 12, 13)).map(_.game).sum}")
  println(s"2 : ${result.map(_.power).sum}")
  bufferedSource.close
  println("Done")


