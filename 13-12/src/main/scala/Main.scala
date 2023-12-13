import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._

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


class Block(input: List[String]):
  var content: Array[Array[Char]] = input.toArray.map(_.toArray)

  def this(from: Array[Array[Char]]) = this(from.map(_.mkString).toList)

  def width: Int = content(0).length
  def height: Int = content.length

  def change(lineIndex: Int)(colIndex: Int): Block =
    val newArray = content.map(_.clone())
    newArray(lineIndex)(colIndex) = content(lineIndex)(colIndex) match
      case '.' => '#'
      case '#' => '.'
      case value => value
    Block(newArray)

  def all: List[Block] =
    val result = for i <- 0 until height
                     j <- 0 until width
    yield
      change(i)(j)

    result.toList

  def toList: List[String] =
    content.map(_.mkString).toList

  override def toString =
    content.map(_.mkString(" ")).mkString("\n")


object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq
    val blocks = lines.map { line =>
      line match
        case value if value.isEmpty => ";"
        case value => value + ","
    }.mkString.split(";").map(_.split(",").toList)

    /*blocks.map(currentBlock => (1 to currentBlock.length).filter(isMirroringVerticallyOnIndex(currentBlock, _)).filterNot(_ == 1).foreach(println))
    println("***********************")
    blocks.map(_.transpose.map(_.mkString)).map(currentBlock => (1 to currentBlock.length).filter(isMirroringVerticallyOnIndex(currentBlock, _)).filterNot(_ == 1).foreach(result => println(s"result is $result")))
    println("$$$$$$$$$$$$$$$$$$$$$$$$")*/
    //blocks(0).transpose.map(_.mkString).map(println)

    println(Block(blocks(0)).all.head)
    println("***********************")
    println("***********************")
    println(Block(blocks(0)).all.drop(61).head)
    println(resolve(Block(blocks(0)).all.drop(61).head.toList))

    println("***********************")
    //val result = ""

    val result = blocks.map(Block.apply(_)).map { currentBlock =>
      val (lineList, colList) = resolve(currentBlock.toList)
      val (line, col) = (lineList.headOption.getOrElse(0), colList.headOption.getOrElse(0))

      //println(s"sizeOf all ${currentBlock.all.length}")
      val result = currentBlock.all.map(current => resolve(current.toList)).filterNot(_ == (0,0)).distinct
      //println(s" toto $result vs ($line, $col) ${result.filterNot(_ == (line, col))}")

      val (newLine, newCol) = result.filterNot(_ == (line, col)).headOption.getOrElse((line, col))
      val finalLine = newLine match
        case value if value == line  => 0
        case _ => newLine
      val finalCol = newCol match
        case value if value == col => 0
        case _ => newCol
      //println(s" toto $result vs ($line, $col) ${result.filterNot(_ == (line, col))} : => ($newLine, $newCol) => ($finalLine, $finalCol)")
      println(s" old ($line, $col)  => ($finalLine, $finalCol)")
      finalLine+finalCol*100
    }.sum


    val (result1, result2) = (s"${result}", "")

    (s"${result1}", s"${result2}")

def allBlocksPossible(currentBlock: List[String]): List[List[String]] =
  val width = currentBlock.length
  val height =  currentBlock(0).length
  //println(s"$width $height")
  val result = for i <- 0 until width
      j <- 0 until height
  yield
    //println(s"$i $j ${currentBlock(i)(j)} ")
    val newChar = currentBlock(i)(j) match
      case '.' => '#'
      case '#' => '.'
    val (beforeChange, afterChange) = currentBlock.mkString(",").splitAt(i+(j*height+j))
    afterChange.length match
      case 0 => (beforeChange.dropRight(1) + newChar).split(",").toList
      case _ => (beforeChange + newChar + afterChange.drop(1)).split(",").toList

  result.foreach(current => println(s" ${current.length} <= $currentBlock"))

  result.toList

def resolve(currentBlock: List[String]): (List[Int], List[Int]) =
  val forLines = (0 to currentBlock(0).length).filter(isMirroringVerticallyOnIndex(currentBlock, _) > 0).map { value => value }.toList
  //(0 to currentBlock(0).length).map(findBadCharacter(currentBlock, _)).filterNot(_ == (-1,-1)).foreach(println)
  val tranposedBlock = currentBlock.transpose.map(_.mkString)
  val forColums = (0 to tranposedBlock(0).length).filter(isMirroringVerticallyOnIndex(tranposedBlock, _) > 0).map { value => value }.toList
  //(0 to tranposedBlock(0).length).map(findBadCharacter(tranposedBlock, _)).filterNot(_ == (-1,-1)).foreach(println)

  (forLines, forColums)

def isMirroringVerticallyOnIndex(input: List[String], index: Int): Int =
  val asColumns = input.transpose.map(_.mkString)
  val (first, second) = asColumns.splitAt(index)

  (first, second) match
    case (Nil, _) => 0
    case (_, Nil) => 0
    case _ =>
      val result = first.reverse.zip(second).filter(_ == _).length == min(first.length, second.length)
      if (result)
        index
      else
        0

def findBadCharacter(input: List[String], index: Int): (Int, Int) =
  val asColumns = input.transpose.map(_.mkString)
  val (first, second) = asColumns.splitAt(index)

  (first, second) match
    case (Nil, _) => (-1, -1)
    case (_, Nil) => (-1, -1)
    case _ =>
      if (first.reverse.zip(second).filter(_ == _).length == min(first.length, second.length) - 1)
        first.reverse.zip(second).zipWithIndex.filter(current => current._1._1 != current._1._2).map(current => (index - current._2, current._2 + index)).head
      else
        (-1, -1)


  //result && math.min(index-1, asColumns.length - (index-1)) != 0
