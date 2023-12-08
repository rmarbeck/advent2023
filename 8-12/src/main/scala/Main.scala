import scala.annotation.tailrec
import scala.io.Source
import scala.math.*

// Right :-/ result is

@main def hello: Unit =
  println("Launching 5-12")
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
    val directions = lines.head.map {
      _ match
        case 'L' => Left
        case 'R' => Right
    }
    val paths = lines.tail.filterNot(_.isEmpty).map(value => value.filter(_.isLetterOrDigit).mkString)

    val result1 = find("1", directions, paths)
    val result2 = find("2", directions, paths)



    (s"${result1}", s"${result2}")

sealed trait Direction

object Left extends Direction
object Right extends Direction

def find(part: String, directions: Seq[Direction], paths: Seq[String]): Long =

  def pgcd(first: Long, second: Long): Long =
    second match
      case 0 => first
      case _ =>
        val modulo = first % second
        if (modulo.equals(0)) second else pgcd(second, modulo)

  def ppcm(first: Long, second: Long): Long =
    (first * second) / pgcd(first, second)

  def ppcmRec(values : Seq[Long]): Long =
    values.length match
      case 2 => ppcm(values(0), values(1))
      case _ => ppcm(values.head, ppcmRec(values.tail))

  @tailrec
  def findPart1(toFind: String, steps: Int): Int =
    val (result, newToFind) = toFind match {
        case "ZZZ" => (Some(steps), "")
        case _ => paths.find(_.startsWith(toFind)).map { value =>
          directions(steps%directions.length) match
            case Left => (None, value.drop(3).take(3))
            case Right => (None, value.drop(6))
        }.getOrElse((None, ""))
      }
    result match
      case Some(result) => result
      case _ => findPart1(newToFind, steps+1)

  @tailrec
  def findPart2(toFind: Seq[String], steps: Int): Int =
    def nextElementList: Seq[String] =
      toFind.map { toFindElement =>
        paths.find(_.startsWith(toFindElement)).map { value =>
          directions(steps % directions.length) match
            case Left => value.drop(3).take(3)
            case Right => value.drop(6)
        }.getOrElse("")
      }
    end nextElementList

    //println(s" =====> $toFind")
    val (result, newToFind) = toFind.map(_.takeRight(1)).filterNot(_=="Z").length match {
      case 0 => (Some(steps), Seq())
      case _ => (None, nextElementList)
    }
    result match
      case Some(result) => result
      case _ => findPart2(newToFind, steps + 1)

  part match
    case "1" => findPart1("AAA", 0)
    case "2" =>
      ppcmRec((paths.map(_.take(3)).filter(_.drop(2) == "A").map(value => findPart2(value :: Nil, 0)).map(_/directions.length) :+ directions.length).map(_.toLong))
    case _ => -1
