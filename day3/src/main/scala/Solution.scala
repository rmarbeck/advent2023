import scala.annotation.{tailrec, targetName}

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (numbers, symbols) = extractNumbersAndSymbols(inputLines)

    val resultPart1 = getTouching(numbers.toList, symbols.toSet).toList.map(_.value)
    val resultPart2 = findGears(numbers.toList, symbols.filter(_.value == '*').toSet).map(_.ratio)

    val result1 = s"${resultPart1.sum}"
    val result2 = s"${resultPart2.sum}"

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

@tailrec
def findGears(numbers: List[Number], symbols: Set[Symbol], touching: List[GearRatio] = Nil): List[GearRatio] =
  numbers match
    case Nil => touching
    case _ =>
      symbols match
        case value if value == Set.empty => touching
        case currentSetOfSymbols =>
          val (head, tail) = (currentSetOfSymbols.head, currentSetOfSymbols.tail)
          val (matching, notMatching) = numbers.partition(head touch _)
          matching match
            case first :: second :: Nil => findGears(notMatching, tail, touching :+ GearRatio(first, second))
            case _ => findGears(notMatching, tail, touching)

case class GearRatio(firstGear: Number, secondGear: Number):
  lazy val ratio = (firstGear * secondGear).value

case class Position(row: Int, col: Int)

sealed trait Element(val pos: Position):
  private val rowMin, rowMax: Int = pos.row
  def colMin: Int
  def colMax: Int
  private lazy val rows = (rowMin, rowMax)
  private lazy val cols = (colMin, colMax)
  def touch(other: Element): Boolean =
    def connect(first: (Int, Int), second: (Int, Int)): Boolean =
      val (firstMin, firstMax, secondMin, secondMax) = (first._1, first._2, second._1, second._2)
      firstMax - secondMax match
        case value if value < -1 => secondMin <= firstMax + 1
        case value if value > 1 => firstMin <= secondMax + 1
        case _ => true

    val rowsConnect = connect(rows, other.rows)
    val colsConnect = connect(cols, other.cols)

    colsConnect && rowsConnect


case class Number(value: Int, position: Position) extends Element(position):
  @targetName("add")
  def +(added: Int): Number = this.copy(value = value + added)
  @targetName("multiply")
  def *(multiplied: Int): Number = this.copy(value = value * multiplied)
  @targetName("multiply")
  def *(other: Number): Number = this.copy(value = value * other.value)
  override val colMin = position.col
  override val colMax = colMin + value.toString.size - 1
case class Symbol(value: Char, position: Position) extends Element(position):
  override val colMin = position.col
  override val colMax = colMin

def extractNumbersAndSymbols(inputString: Seq[String]): (Seq[Number], Seq[Symbol]) =
  val numbers =
    inputString.zipWithIndex.flatMap:
      case (line, row) => line.zipWithIndex.filter(_._1.isDigit).foldLeft((List[Number](), -1)):
        case ((acc, nextIndexExpected), (digit, index)) if index == nextIndexExpected => ((acc.head * 10 + digit.asDigit) +: acc.tail, index + 1)
        case ((acc, nextIndexExpected), (digit, index)) => (Number(digit.asDigit, Position(row, index)) +: acc, index + 1)
      ._1

  val symbols =
    inputString.zipWithIndex.flatMap:
      case (line, row) =>
        line.zipWithIndex
          .filterNot(_._1.isDigit)
          .filterNot(_._1 == '.')
          .map((symbol, index) => Symbol(symbol, Position(row, index)))

  (numbers,symbols)