import scala.collection.immutable.BitSet

type CardNumbers = Map[Int, Int]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val cards = inputLines.view.collect:
      case CardExt(card) => card

    val cardsNumber = cards.size

    val (result1: Int, result2: Int, _) = cards.map(_.winnings).zipWithIndex.foldLeft((0, 0, Map.empty.withDefault(key => if key < cardsNumber then 1 else 0): CardNumbers)):
      case ((sumPart1, sumPart2, remainingCardNumbers), (winnings, index)) =>
        val current = remainingCardNumbers(index)
        (sumPart1 + winnings.toScore,
          sumPart2 + current,
          remainingCardNumbers - index ++
            (index + 1 to index + winnings).collect:
              case idx if remainingCardNumbers(idx) != 0 => idx -> (remainingCardNumbers(idx) + current)
          )

    (s"$result1", s"$result2")

case class Card(winning: BitSet, draw: BitSet):
  lazy val winnings: Int = (draw intersect winning).size
  lazy val score: Int = winnings.toScore

extension (self: Int)
  def toScore: Int =
    self match
      case 0 => 0
      case value => math.pow(2, value - 1).toInt

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
        case values => Some(values.map(_.toInt).to(BitSet))