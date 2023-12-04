import scala.io.Source
import scala.math._

// Right :-/ result is

@main def hello: Unit =
  println("Launching 1-12-2")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val lines = bufferedSource.getLines().toSeq
  val scores = lines.zipWithIndex.map: (line, index) =>
    line match
      case s"Card ${number}: ${winnings} | ${ours}" => scorePart1(winnings, ours)
  val result1 = scores.sum
  println(s"1 : $result1")
  val scores2 = lines.map:
      case s"Card ${number}: ${winnings} | ${ours}" => score(winnings, ours)
  .zipWithIndex
  .foldLeft(List.fill(lines.length)(1)): (acc, scoreAndIndex) =>
    acc.zip(calculateToAddList(acc, scoreAndIndex)).map(_ + _)
  val result2 = scores2.sum
  println(s"2 : ${result2}")
  bufferedSource.close
  println("Done")

def calculateToAddList(current: List[Int], scoreAndIndex: (Int, Int)): List[Int] =
  val (score, index) = scoreAndIndex
  val valToAdd = current(index)
  List.fill(current.length){0}.zipWithIndex.map:(_, innerIndex) =>
    innerIndex match
      case value if value > index && value <= (index + score) => valToAdd
      case _ => 0
def scorePart1(winnings: String, ours: String): Int =
  Math.pow(2, score(winnings, ours)-1).toInt

def score(winnings: String, ours: String): Int =
  val oursCleaned = clean(ours)
  clean(winnings).split(',').map:
    case value if oursCleaned.split(',').find(_.equals(value)).isDefined => 1
    case _ => 0
  .sum
end score

def clean(toClean: String): String =
  toClean.trim.replaceAll(" +", " ").replace(" ", ",")