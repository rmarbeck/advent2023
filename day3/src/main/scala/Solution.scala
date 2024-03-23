import scala.annotation.{tailrec, targetName}

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (numbers, symbols) = extractNumbersAndSymbols(inputLines)

    val resultPart1 = getTouching(numbers, symbols).foldLeft(0)((acc, number) => acc + number.value)
    val resultPart2 = findGears(numbers, symbols.filter(_.value == '*')).foldLeft(0)((acc, number) => acc + number.ratio)

    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def getTouching(numbers: Seq[Number], symbols: Seq[Symbol], touching: Set[Number] = Set()): Set[Number] =
  (numbers.size, symbols.size) match
    case (0, _) | (_, 0) => touching
    case _ =>
      val (head, tail) = (symbols.head, symbols.tail)
      val (matching, notMatching) = numbers.partition(head touch _)
      getTouching(notMatching, tail, touching ++ matching.toSet)

@tailrec
def findGears(numbers: Seq[Number], symbols: Seq[Symbol], touching: List[GearRatio] = Nil): List[GearRatio] =
  (numbers.size, symbols.size) match
    case (0, _) | (_, 0) => touching
    case _ =>
      val (head, tail) = (symbols.head, symbols.tail)
      val (matching, notMatching) = numbers.partition(head touch _)
      matching.size match
        case 2 => findGears(notMatching, tail, touching :+ GearRatio(matching.head, matching.last))
        case _ => findGears(notMatching, tail, touching)

case class GearRatio(firstGear: Number, secondGear: Number):
  lazy val ratio = (firstGear * secondGear).value

case class Position(row: Int, col: Int):
  def follows(other: Position, of: Int): Boolean =
    (row - other.row, col - other.col) match
      case (0, drift) if drift == of => true
      case _ => false

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
  val numbersAndSymbols =
    for
      case (line, row) <- inputString.zipWithIndex
      case (char, col) <- line.zipWithIndex
      if char != '.'
    yield
      char match
        case digit if digit.isDigit => Number(digit.asDigit, Position(row, col))
        case symbol => Symbol(symbol, Position(row, col))

  val (numbers, symbols) =
    numbersAndSymbols.foldLeft((Nil: List[Number], Nil: List[Symbol])):
      case (acc, number: Number) =>
        acc._1 match
          case Nil => (List(number), acc._2)
          case head :: tail =>
            if (number.position follows(head.position, head.value.toString.size))
              (head.copy(value = head.value * 10 + number.value) +: tail, acc._2)
            else
              (number +: acc._1, acc._2)
      case (acc, symbol: Symbol) => (acc._1, symbol +: acc._2)

  (numbers, symbols)


def extractNumbersAndSymbols2(inputString: Seq[String]): (Seq[Number], Seq[Symbol]) =
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
          .withFilter(!_._1.isDigit)
          .withFilter(_._1 != '.')
          .map((symbol, index) => Symbol(symbol, Position(row, index)))

  (numbers,symbols)