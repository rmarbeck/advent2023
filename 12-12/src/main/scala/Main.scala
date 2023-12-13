import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._
import scala.util.matching.Regex
import scala.collection.parallel._
import collection.parallel.CollectionConverters.seqIsParallelizable

import java.time.Duration
import java.time.Instant
// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching X-12")
  val startTime = Instant.now()

  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")
  println(s"Computing time is ${Duration.between(startTime, Instant.now()).toMillis}ms")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq
    val symbols = lines.map(_.span(_!=' ')._1)
    val targets = lines.map(_.span(_!=' ')._2.split(',').map(_.trim.toInt).toSeq)

    val other = (symbols.zip(targets)).par.map { (symbol, target) =>
      //println(s"${symbol} -> ${target.toList} = ${splitAndResolve(symbol, target.toList)}")
      val optimized = simplifyLazyList(symbol, target.toList).lastOption.getOrElse((symbol, target.toList))
      /*if (optimized._1 != symbol)
        println(s"optimization ($symbol, ${target.toList}) => $optimized")
      else
        println(s"not optimized ($symbol, ${target.toList})")*/
      brutForce(optimized._1, optimized._2)
    }.sum

    val result = ""/*symbols.zip(targets).map { (symbol, target) =>
      println(s"${symbol} -> ${target.toList} = ${brutForce(symbol, target.toList)}")
      brutForce(symbol, target.toList)
    }.sum*/
    /*symbols.foreach { line =>
      println(asGroups(line))
    }*/

    /*println(s"1 => ${maxSolutionsBy(2,2, 10)}")
    println(s"1 => ${maxSolutionsBy(3,2, 5)}")

    println(s"1,1,3 => ${maxSolutionsBy(2,2, 2)}")
    println(s"1,1,3 bis => ${maxSolutionsBy(2,2, 9)}")
    println(s"1,1,3 quater => ${2*maxSolutionsBy(0,2, 1)}")
    println(s"1,3,1,6 => ${maxSolutionsBy(1,2, 4)}")
    //println(s"3,2,1 => ${maxSolutionsBy(1,2, 4)}")
    println(s"3,2,1 => ${maxSolutionsBy(1,2, 4)}")*/

    val (result1, result2) = (result, other)



    (s"${result1}", s"${result2}")

def asGroups(input: String): Seq[Int] =
  input.foldLeft(Seq(0)) {(acc, newValue) =>
    newValue match
      case '.' if acc.last == 0 => acc
      case '.' => acc :+ 0
      case _ => acc.dropRight(1) :+ acc.last+1
  }.filterNot(_==0)

def maxOutsByListOfGroups(listOfGroups: Seq[Int], numberOfDots: Int): Int =
  maxSolutionsBy(listOfGroups.length-1, 2, numberOfDots)

def maxSolutionsBy(numberOfNonZeroVars: Int, numberOfZeroVars: Int, numberOfDots: Int): Int =
  numberOfNonZeroVars match {
    case value if value >= 1 =>
      (1 to (numberOfDots-(value-1))).map { current =>
        maxSolutionsBy(numberOfNonZeroVars - 1, numberOfZeroVars + 1, numberOfDots - current)
      }.sum
    case _ => numberOfZeroVars match {
      case value if value > 2 =>
        (0 to numberOfDots).map { current =>
          maxSolutionsBy(0, numberOfZeroVars - 1, numberOfDots - current)
        }.sum
      case _ => numberOfDots + 1
    }
  }

def splitAndResolve(initialLine: String, target: Seq[Int]): Int =
  if (target.length == 0)
    1
  else
    val trimmed = initialLine.replace('.', ' ').trim.replace(' ', '.')
    //trimmed.split('.').filterNot(_.isEmpty).map(guessAndResolveHead(_, target)).sum
    val result = trimmed.split('.').filterNot(_.isEmpty).headOption.map { firstPart =>
      guessAndResolveHead(firstPart, target).match {
          case None => 0 + splitAndResolve(initialLine.drop(firstPart.length), target)
          case Some(Seq()) => 1 * splitAndResolve(initialLine.drop(firstPart.length), target.tail)
          case Some(value) => value.map { subPart =>
            1 * splitAndResolve(subPart+"."+trimmed.split('.').filterNot(_.isEmpty).tail.mkString("."), target.tail)
          }.sum
      }
    }.sum
    //println(s"$initialLine $target => ${trimmed.split('.').filterNot(_.isEmpty).toList} .... ${trimmed.split('.').filterNot(_.isEmpty).headOption} -> $result")
    result

def guessAndResolveHead(currentWithoutDots: String, target: Seq[Int]): Option[Seq[String]] =
  val result = currentWithoutDots.length match
    case value if value < target.head => None
    case value if value == target.head => Some(Seq())
    case value if value == target.head + 1 && currentWithoutDots.head != '#' && currentWithoutDots.last == '#' => Some(Seq())
    //case value if !currentWithoutDots.startsWith("#") && currentWithoutDots.contains("#"*target.head) && currentWithoutDots.indexOf("#") < target.head => Some(Seq(currentWithoutDots.drop(currentWithoutDots.indexOf("#")+target.head+1)))
    case value => Some((0 to value-target.head).map(current => currentWithoutDots.drop(target.head+current+1/*the '.' to separate*/)).toList)
  result

def brutForce(current: String, target: Seq[Int]): Int =
  current.contains('?') match
    case false =>
      asGroups(current) match
        case value if value == target => 1
        case _ => 0
    case _ => brutForce(current.replaceFirst("\\?", "#"), target)+brutForce(current.replaceFirst("\\?", "."), target)

def simplifyLazyList(input: String, target: Seq[Int]): LazyList[(String, Seq[Int])] =
  val computed = simplifyInput(input, target)
  if (checkEquality((computed._1, computed._2), (input, target))) LazyList.empty
  else LazyList.cons((computed._1, computed._2), simplifyLazyList(computed._1, computed._2))

def checkEquality(computed: (String, Seq[Int]), old: (String, Seq[Int])): Boolean =
  //println(s"$computed vs $old => ${computed == old}")
  computed == old

def simplifyInput(input: String, target: Seq[Int]): (String, Seq[Int]) =
  val endWithSharp: Regex = """^(.*)\.([#]+[\.]*)$""".r
  val startWithSharp: Regex = """^([\.]*[#]+[\.]*)\.(.*)$""".r

  input match
    case value if value.startsWith(".") => (input.replaceFirst("[\\.]*", ""), target)
    case value if endWithSharp.findFirstIn(input).isDefined =>
      input match {
        case endWithSharp(start, _) => (start+".", target.dropRight(1))
      }
    case value if startWithSharp.findFirstIn(input).isDefined =>
      input match {
        case startWithSharp(_, end) => ("."+end, target.drop(1))
      }
    case value if value.contains("?") => simplify2(input, target, 0)
    case value if value.contains("?") => simplify3(input, target, 0)
    case _ => (input, target)

def simplify2(input: String, target: Seq[Int], start: Int): (String, Seq[Int]) =
  def transformAtIndex(index: Int): String =
    val (start, end) = input.splitAt(index)
    start + "." + end.drop(1)
  def replaceAtIndex(index: Int): String =
    val (start, end) = input.splitAt(index)
    (start + "#" + end.drop(1)).replace("?", ".")
  def findAll: Seq[Int] =
    input.zipWithIndex.filter(_._1 == '?').map(_._2)

  findAll.find(currentIndex => isImpossible(replaceAtIndex(currentIndex), target)).map(index => (transformAtIndex(index), target)).getOrElse((input, target))

def isImpossible(input: String, target: Seq[Int]): Boolean =
  asGroups(input).maxOption.flatMap(outer => target.maxOption.map(outer > _)).getOrElse(false)

def simplify3(input: String, target: Seq[Int], start: Int): (String, Seq[Int]) =
  def transformAtIndex(index: Int): String =
    val (start, end) = input.splitAt(index)
    start + "#" + end.drop(1)
  def replaceAtIndex(index: Int): String =
    val (start, end) = input.splitAt(index)
    (start + "." + end.drop(1)).replace("?", "#")
  def findAll: Seq[Int] =
    input.zipWithIndex.filter(_._1 == '?').map(_._2)

  findAll.find(currentIndex => isImpossible2(replaceAtIndex(currentIndex), target)).map(index => (transformAtIndex(index), target)).getOrElse((input, target))

def isImpossible2(input: String, target: Seq[Int]): Boolean =
  asGroups(input).headOption.flatMap(outer => target.headOption.map(outer < _)).getOrElse(false)