type CardNumbers = Map[Int, Int]

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val cards = inputLines.collect:
      case CardExt(card) => card

    val result1 = cards.map(_.score).sum

    val cardsNumber = cards.size

    val nbOfEachCards: CardNumbers = cards.map(_.winnings).zipWithIndex.foldLeft(Map.empty.withDefault(key => if key < cardsNumber then 1 else 0): CardNumbers):
      case (acc, (0, index)) => acc
      case (acc, (winnings, index)) =>
        acc ++
          (index + 1 to index + winnings).collect:
            case idx if acc(idx) != 0 => idx -> (acc(idx) + acc(index))

    val result2 = cards.indices.map(nbOfEachCards).sum

    (s"$result1", s"$result2")

case class Card(winning: List[Int], draw: List[Int]):
  lazy val winnings: Int = (draw intersect winning).size
  lazy val score: Int =
    winnings match
      case 0 => 0
      case value => math.pow(2, value - 1).toInt

object CardExt:
  private val CardMatcher = """(\d+): (.+) \| (.+)""".r.unanchored
  def unapply(str: String): Option[Card] =
    str match
      case CardMatcher(cardId, NumbersExt(winnings), NumbersExt(draws)) => Some(Card(winnings, draws))
      case _ => None

  private object NumbersExt:
    def unapply(str: String): Option[List[Int]] =
      """(\d+)""".r.unanchored.findAllIn(str).toList match
        case Nil => None
        case values => Some(values.map(_.toInt))