import scala.io.Source
import scala.math._

// Right :-/ result is

@main def hello: Unit =
  println("Launching 6-12")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq
    val times = lines.head.replaceAll(" +", ",").split(',').tail.map(_.toInt).toSeq
    val distances = lines.tail.head.replaceAll(" +", ",").split(',').tail.map(_.toInt).toSeq

    val result1 = times.zip(distances).map:(b, c) =>
      findRoots(-1, b, -c)
    .fold(1)(_*_)

    val timeForPart2 = times.map(_.toString).mkString.toLong
    val distanceForPart2 = distances.map(_.toString).mkString.toLong
    val result2 = findRoots(-1, timeForPart2, -distanceForPart2)

    (s"${result1}", s"${result2}")

def findRoots(a: Long = 1, b: Long, c: Long): Int =
  val delta: Long = (b * b) - 4 * (a * c)
  val (root1: Double, root2: Double) = ((- b.toFloat - sqrt(delta.toFloat)) / (2 * a.toFloat),(- b.toFloat + sqrt(delta.toFloat)) / (2 * a.toFloat))

  manageRoots(root1, root2)

def manageRoots(root1: Double, root2: Double): (Int) =
  if (root1>root2)
    manageRoots(root2, root1)
  else
    val smallerRoundedToFirstHigherInt = root1%root1.ceil match
      case 0.0 => (root1.ceil + 1).toInt
      case _ => root1.ceil.toInt
    val higherRoundedToFirstLowerInt = root2%root2.floor match
      case 0.0 => (root2.floor - 1).toInt
      case _ => root2.floor.toInt
    higherRoundedToFirstLowerInt - smallerRoundedToFirstHigherInt + 1