object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val cards = inputLines.collect:
      case CardExt(card) => card

    val resultPart1 = cards.map(_.score).sum

    val totalCards = Array.fill(cards.size)(1)
    for
      (currentCard, index) <- cards.zipWithIndex
      winnings = currentCard.winnings
      if winnings != 0
    do
      val numberOfCurrentCard = totalCards(index)
      (index + 1 to index + winnings).foreach:
        case innerIndex =>
          totalCards.isDefinedAt(innerIndex) match
            case true => totalCards(innerIndex) += numberOfCurrentCard
            case false => ()

    val resultPart2 = totalCards.sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Card(winning: List[Int], draw: List[Int]):
  lazy val winnings: Int = (draw intersect winning).size
  lazy val score: Int =
    winnings match
      case 0 => 0
      case value => math.pow(2, value - 1).toInt



object CardExt:

  val CardMatcher = """Card[ ]+(\d+): (.+) \| (.+)""".r
  def unapply(str: String): Option[Card] =
    str match
      case CardMatcher(cardId, NumbersExt(winnings), NumbersExt(draws)) => Some(Card(winnings, draws))
      case _ => None

  object NumbersExt:
    val NumbersMatcher = """(\d+)""".r
    def unapply(str: String): Option[List[Int]] =
      val values =
        for
          case NumbersMatcher(value) <- NumbersMatcher.findAllIn(str).toList
        yield
          value.toInt
      values.size match
        case 0 => None
        case _ => Some(values)