object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val cards = inputLines.toList.map(Card.fromString(_))

    val resultPart1 = cards.map(_.score).sum

    val totalCards = Array.fill(cards.size)(1)
    for
      currentCard <- cards
      winnings = currentCard.winnings
      if winnings != 0
      currentCardIndex = currentCard.id - 1
    do
      val numberOfCurrentCard = totalCards(currentCardIndex)
      (currentCardIndex + 1 to currentCardIndex + winnings).foreach:
        case index =>
          totalCards.isDefinedAt(index) match
            case true => totalCards(index) += numberOfCurrentCard
            case false => ()

    val resultPart2 = totalCards.sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Card(id: Int, winning: List[Int], draw: List[Int]):
  lazy val winnings: Int = (draw intersect winning).size
  lazy val score: Int =
    winnings match
      case 0 => 0
      case value => math.pow(2, value - 1).toInt

object Card:
  def fromString(inputString: String): Card =
    def extractNumbers(partialString: String): List[Int] =
      partialString.split(" ").filterNot(_.isEmpty).map(_.toInt).toList
    inputString match
      case s"Card $id: $winnings | $draw" => Card(id.trim.toInt, extractNumbers(winnings), extractNumbers(draw))
      case _ => throw Exception("Not supported")