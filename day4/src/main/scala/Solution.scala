import scala.collection.immutable.BitSet

type CardsOccurrences = Map[Int, Int]
type Accumulator = (Int, Int, CardsOccurrences)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val cards = inputLines.view.collect:
      case CardExt(card) => card

    val numberOfCards = cards.size
    val start: Accumulator = (0, 0, Map.empty.withDefault(key => if key < numberOfCards then 1 else 0))

    val (result1, result2, _) = cards.map(_.winnings).zipWithIndex.foldLeft(start):
      case ((sumPart1, sumPart2, remainingCardNumbers), (winnings, index)) =>
        val nbOccurrences = remainingCardNumbers(index)
        (
          sumPart1 + winnings.toScore,
          sumPart2 + nbOccurrences,
          remainingCardNumbers - index ++
            (index + 1 to index + winnings).collect:
              case idx if remainingCardNumbers(idx) != 0 => idx -> (remainingCardNumbers(idx) + nbOccurrences)
        )

    (s"$result1", s"$result2")

case class Card(winning: BitSet, draw: BitSet):
  lazy val winnings: Int = (draw intersect winning).size
  lazy val score: Int = winnings.toScore

extension (self: Int)
  def toScore: Int =
    self match
      case 0 => 0
      case value => 1 << (value - 1)

object CardExt:
  private val CardMatcher = """(\d+): (.+) \| (.+)""".r.unanchored
  def unapply(str: String): Option[Card] =
    str match
      case CardMatcher(cardId, NumbersExt(winnings), NumbersExt(draws)) => Some(Card(winnings, draws))
      case _ => None

  private object NumbersExt:
    def unapply(str: String): Option[BitSet] =
      """(\d+)""".r.unanchored.findAllIn(str).toList match
        case Nil => None
        case values => Some(values.foldLeft(BitSet.empty)(_ + _.toInt))