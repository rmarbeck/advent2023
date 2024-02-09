import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (numbers, symbols) = getNumbersAndSymbols(inputLines)

    val resultPart1 = getTouching(numbers.toList, symbols.toSet).toList.map(_.value.toLong)

    val result1 = s"${resultPart1.sum}"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def getTouching(numbers: List[Number], symbols: Set[Symbol], touching: Set[Number] = Set()): Set[Number] =
  numbers match
    case Nil => touching
    case _ =>
      symbols match
        case value if value == Set.empty => touching
        case currentSetOfSymbols =>
          val (head, tail) = (currentSetOfSymbols.head, currentSetOfSymbols.tail)
          val (matching, notMatching) = numbers.partition(head touch _)
          getTouching(notMatching, tail, touching ++ matching)

case class Position(row: Int, col: Int)

sealed trait Element(val pos: Position):
  val rowMin, rowMax: Int = pos.row
  def colMin: Int
  def colMax: Int
  def touch(other: Element): Boolean =
    val rowsConnect = this.rowMax <= other.rowMax + 1 && this.rowMin >= other.rowMin - 1
    val colsConnect = this.colMax <= other.colMax + 1 && this.colMin >= other.colMin - 1
    rowsConnect && colsConnect


case class Number(value: Int, position: Position) extends Element(position):
  def +(added: Int) = this.copy(value = value + added)
  def *(multiplied: Int) = this.copy(value = value * multiplied)
  override val colMin = position.col
  override val colMax = colMin + value.toString.size - 1
case class Symbol(value: Char, position: Position) extends Element(position):
  override val colMin = position.col
  override val colMax = colMin

def getNumbersAndSymbols(inputString: Seq[String]): (Seq[Number], Seq[Symbol]) =
  val numbers =
    inputString.zipWithIndex.flatMap:
      case (line, row) => line.zipWithIndex.filter(_._1.isDigit).foldLeft((List[Number](), -1)):
        case ((acc, nextIndexExpected), (digit, index)) if index == nextIndexExpected => ((acc.head * 10 + digit.asDigit) +: acc.tail, index + 1)
        case ((acc, nextIndexExpected), (digit, index)) => (Number(digit.asDigit, Position(row, index)) +: acc, index + 1)
      ._1

  val symbols =
    inputString.zipWithIndex.flatMap:
      case (line, row) => line.zipWithIndex.filterNot(_._1.isDigit).filterNot(_._1 == '.').map((symbol, index) => Symbol(symbol, Position(row, index)))

  (numbers,symbols)