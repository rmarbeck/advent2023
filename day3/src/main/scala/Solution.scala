import scala.annotation.{tailrec, targetName}
import scala.util.matching.Regex

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given NumberAndSymbolsExtractor = RegExpExtractor

    val (numbers, symbols) = summon[NumberAndSymbolsExtractor].from(inputLines)

    // As it is a set, using foldleft and not mapping to then summing
    val resultPart1 = getTouching(numbers, symbols).foldLeft(0)((acc, number) => acc + number.value)
    val resultPart2 = findGears(numbers, symbols.filter(_.value == '*')).map(_.ratio).sum

    val result1 = s"${resultPart1}"
    val result2 = s"${resultPart2}"

    (s"${result1}", s"${result2}")

end Solution

// Two parameters (adding a second one for touching) to avoid closure to use value of initial parameter
type Updater[A] = (Seq[Number], A) => A
type UpdaterPart1 = Updater[Set[Number]]
type UpdaterPart2 = Updater[List[GearRatio]]

@tailrec
def search[A](numbers: Seq[Number], symbols: Seq[Symbol], touching: A)(using touchingUpdater: Updater[A]): A =
  numbers.size*symbols.size match
    case 0 => touching
    case _ =>
      val (headingSymbol, remainingSymbols) = (symbols.head, symbols.tail)
      val (matchingNumbers, notMatchingNumbers) = numbers.partition(headingSymbol touch _)
      search(notMatchingNumbers, remainingSymbols, touchingUpdater.apply(matchingNumbers, touching))

def getTouching(numbers: Seq[Number], symbols: Seq[Symbol], touching: Set[Number] = Set()): Set[Number] =
  given UpdaterPart1 = (matching, currentTouching) => currentTouching ++ matching.toSet
  search(numbers, symbols, touching)

def findGears(numbers: Seq[Number], symbols: Seq[Symbol], touching: List[GearRatio] = Nil): List[GearRatio] =
  given UpdaterPart2 = (matching, currentTouching) =>
    matching.size match
      case 2 => GearRatio(matching.head, matching.last) +: currentTouching
      case _ => currentTouching
  search(numbers, symbols, touching)

case class GearRatio(firstGear: Number, secondGear: Number):
  lazy val ratio = (firstGear * secondGear).value

case class Position(row: Int, col: Int):
  def follows(other: Position, of: Int): Boolean =
    (row - other.row, col - other.col) match
      case (0, drift) if drift == of => true
      case _ => false

sealed trait Element(val pos: Position):
  private val rowMin, rowMax: Int = pos.row
  def size : Int = colMax - colMin
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

/**
 * Different extractors to parse input
 *
 * Including a regex one with Scala Extractors using unapply
 *
 */

trait NumberAndSymbolsExtractor:
  def from(inputLines: Seq[String]): (Seq[Number], Seq[Symbol]) =
    val results = for
      (line, index) <- inputLines.zipWithIndex
    yield
      from(line, index)
    results.fold((Seq[Number](), Seq[Symbol]())):
      case ((prevNumber, prevSymbol), (nextNumber, nextSymbol)) => (prevNumber ++ nextNumber, prevSymbol ++ nextSymbol)

  def from(singleLine: String, rowIndex: RowIndex): (Seq[Number], Seq[Symbol])

type RowIndex = Int

object NumberExt:
  def unapply(str: String): Option[Int] =
    str.toIntOption

  def unapply(matchStr: Regex.Match)(using rowIndex: RowIndex): Option[Number] =
    unapply(matchStr.matched) match
      case Some(value) => Some(Number(value, Position(rowIndex, matchStr.start)))
      case None => None

object SymbolExt:
  def unapply(str: String): Option[Char] =
    str.size match
      case 1 =>
        str.head match
          case '.' => None
          case value if value.isDigit => None
          case value => Some(value)
      case _ => None

  def unapply(matchStr: Regex.Match)(using rowIndex: RowIndex): Option[Symbol] =
    unapply(matchStr.matched) match
      case Some(value) => Some(Symbol(value, Position(rowIndex, matchStr.start)))
      case None => None

object RegExpExtractor extends NumberAndSymbolsExtractor:
  private val matcher = """(\d+)|(\.+)|([^0-9\.]+)""".r

  override def from(singleLine: String, rowIndex: Int): (Seq[Number], Seq[Symbol]) =
    given RowIndex = rowIndex
    matcher.findAllMatchIn(singleLine).foldLeft((Seq[Number](), Seq[Symbol]())):
      case (acc, NumberExt(number)) => (number +: acc._1, acc._2)
      case (acc, SymbolExt(symbol)) => (acc._1, symbol +: acc._2)
      case (acc, other) => (acc._1, acc._2)

object ByCharacterExtractor extends NumberAndSymbolsExtractor:
  override def from(singleLine: String, rowIndex: RowIndex): (Seq[Number], Seq[Symbol]) =
    from(Seq(singleLine))
  override def from(inputString: Seq[String]): (Seq[Number], Seq[Symbol]) =
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

object SimplerByCharacterExtractor extends NumberAndSymbolsExtractor:
  override def from(singleLine: String, rowIndex: RowIndex): (Seq[Number], Seq[Symbol]) =
    from(Seq(singleLine))
  override def from(inputString: Seq[String]): (Seq[Number], Seq[Symbol]) =
    val numbersAndSymbols =
      for
        case (line, row) <- inputString.zipWithIndex
        case (char, col) <- line.zipWithIndex
        if char != '.'
      yield
        char match
          case digit if digit.isDigit => Number(digit.asDigit, Position(row, col))
          case symbol => Symbol(symbol, Position(row, col))

    val numbers =
      numbersAndSymbols.collect:
        case number: Number => number
      .foldLeft(Nil : List[Number]):
        (acc, number: Number) =>
          acc match
            case Nil => List(number)
            case head :: tail =>
              if (number.position follows(head.position, head.value.toString.size))
                head.copy(value = head.value * 10 + number.value) +: tail
              else
                number +: acc

    val symbols = numbersAndSymbols.collect:
      case symbol: Symbol => symbol

    (numbers, symbols)

object InitialExtractor extends NumberAndSymbolsExtractor:
  override def from(singleLine: String, rowIndex: RowIndex): (Seq[Number], Seq[Symbol]) =
    from(Seq(singleLine))
  override def from(inputString: Seq[String]): (Seq[Number], Seq[Symbol]) =
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