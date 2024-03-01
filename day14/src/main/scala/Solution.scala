import scala.annotation.tailrec


object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val numberOfCycles = 1_000_000_000

    val values = inputLines.map(_.toCharArray).toArray
    val resultPart1 = values.transpose.map(countMovingUp).sum

    val panel = Panel.fromUnTransposed(values)

    val (cycle, drift) = findCycleWithHash(countsWithHash(panel))
    
    val resultPart2 = cycle((numberOfCycles - drift) % cycle.size - 1)

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def findCycle(provider: LazyList[Int], currentList: List[Int] = Nil): (List[Int], Int) =
  val minimumSize = 5
  def cycleSize: Option[Int] =
    currentList.size match
      case value if value < minimumSize * 2 => None
      case sizeOfList =>
        (sizeOfList / 2 until minimumSize by -1).find:
          slizeSize =>
            val (head, tail) = currentList.splitAt(slizeSize)
            tail.indexOfSlice(head) == 0

  cycleSize match
    case Some(value) => (currentList.take(value).reverse, currentList.size - value)
    case None => findCycle(provider.tail, provider.head +: currentList)

@tailrec
def findCycleWithHash(provider: LazyList[(Int, Long)], currentList: List[(Int, Long)] = Nil): (List[Int], Int) =
  def cycleSize: Option[Int] =
    currentList.map(_._2).indexOf(provider.head._2) match
      case -1 => None
      case value => Some(value + 1)

  cycleSize match
    case Some(value) => (currentList.map(_._1).take(value).reverse, currentList.size)
    case None => findCycleWithHash(provider.tail, provider.head +: currentList)

def counts(panel: Panel): LazyList[Int] =
  val afterOneCycle = panel.turn4TimesTilting
  afterOneCycle.count #:: counts(afterOneCycle)

def countsWithHash(panel: Panel): LazyList[(Int, Long)] =
  val afterOneCycle = panel.turn4TimesTilting
  afterOneCycle.countWithHash #:: countsWithHash(afterOneCycle)

private case class Panel(rocks: Array[Array[Char]]):
  private lazy val northOriented = rocks.transpose

  lazy val height = rocks.length
  lazy val width = rocks(0).length

  lazy val count = rocks.map(countInPlace).sum

  private lazy val rotate = Panel {
    Array.tabulate(height, width):
      (row, col) => rocks(col)(width - row - 1)
  }

  private lazy val reorganized: Panel = Panel(rocks.map(reorganize))

  def turn4TimesTilting: Panel =
    (1 to 4).foldLeft(this):
      (acc, _) => acc.reorganized.rotate

  lazy val countHash: Long =
    (for
      row <- 0 until height
      col <- 0 until width
      if rocks(row)(col) == 'O'
    yield
      (row+1)*(col+1)
    ).sum

  lazy val countWithHash = (count, countHash)

  override def toString: String = northOriented.map(_.mkString).mkString("\n")

object Panel:
  def fromUnTransposed(unTransposedRocks: Array[Array[Char]]): Panel = Panel(unTransposedRocks.transpose)

def reorganize(rocks: Array[Char]): Array[Char] =
  val reorganized = Array.tabulate(rocks.size)(rocks(_))
  var counter = 0
  for
    (currentChar, index) <- rocks.zipWithIndex
    if currentChar != '.'
  do
    currentChar match
      case '#' =>
        counter = index + 1
      case 'O' =>
        if (counter != index)
          reorganized(counter) = 'O'
          reorganized(index) = '.'
        counter += 1

  reorganized

def countInPlace(rocks: Array[Char]): Int =
  val size = rocks.size
  rocks.zipWithIndex.filter(_._1 == 'O').map(size - _._2).sum

def countMovingUp(rocks: Array[Char]): Int =
  val size = rocks.size
  countMovingUp(rocks.toList, size, size, 0)

@tailrec
def countMovingUp(rocks: List[Char], counter: Int, index: Int, acc: Int): Int =
  rocks match
    case Nil => acc
    case head :: tail =>
      head match
        case 'O' => countMovingUp(tail, counter - 1, index - 1, acc + counter)
        case c@ ('.' | '#') =>
          tail.indexWhere(_ != c) match
            case -1 => acc
            case value =>
              val newCounter = c match
                case '.' => counter
                case '#' => index - (value + 1)
              countMovingUp(tail.drop(value), newCounter, index - (value + 1), acc)