import scala.collection.immutable.HashMap

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val handsWithBids = inputLines.map:
      case s"$hand $bid" => HandWithBid(Hand(hand), bid.toLong)

    val orderingOfHandsPart1 =
      import Card.part1Ordering
      import Hand.part1Ordering
      handsWithBids.sortBy(_.hand)

    val orderingOfHandsPart2 =
      import Card.part2Ordering
      import Hand.part2Ordering
      handsWithBids.sortBy(_.hand)

    val List(resultPart1, resultPart2) = List(orderingOfHandsPart1, orderingOfHandsPart2).map:
      case sortedHands => sortedHands.zipWithIndex.map((handWithBid, index) => handWithBid.bid * (index + 1)).sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Card(char: Char):
  val List(ordinalPart1, ordinalPart2) = List(Card.ordinalsPart1, Card.ordinalsPart2).map:
    case ordinals => ordinals.get(char) match
      case Some(value) => value
      case None => throw Exception(s"Not valid card : $char")

object Card:
  private def asHashMap(orderedCards: List[Char]): Map[Char, Int] =
    HashMap[Char, Int](orderedCards.zipWithIndex.map:
      case (char, index) => char -> index
    : _*)
  private val letters: List[Char] = List('A', 'K', 'Q', 'J', 'T')
  private val ordinalsPart1: Map[Char, Int] = asHashMap(letters ::: (9 to 2 by -1).map(_.toString.head).toList)
  private val ordinalsPart2: Map[Char, Int] = asHashMap(letters.filterNot(_ == 'J') ::: (9 to 2 by -1).map(_.toString.head).toList ::: List('J'))

  given part1Ordering: Ordering[Card] with
    def compare(first: Card, second: Card): Int =
      first.ordinalPart1.compare(second.ordinalPart1)

  given part2Ordering: Ordering[Card] with
    def compare(first: Card, second: Card): Int =
      first.ordinalPart2.compare(second.ordinalPart2)

enum TypeOfHand:
  case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard

export TypeOfHand.*

case class Hand(raw: String):
  lazy val cards: List[Card] = raw.map(Card(_)).toList
  val typeOfHandPart1: TypeOfHand =
    val grouped = raw.groupMapReduce(identity)(_ => 1)(_ + _)
    (grouped.values.max, grouped.values.product) match
      case (5, _) => FiveOfAKind
      case (4, _) => FourOfAKind
      case (3, 6) => FullHouse
      case (3, _) => ThreeOfAKind
      case (2, 4) => TwoPair
      case (2, _) => OnePair
      case _ => HighCard

  val typeOfHandPart2: TypeOfHand =
    val numberOfJs = raw.count(_ == 'J')
    val grouped = raw.groupMapReduce(identity)(_ => 1)(_ + _)
    (grouped.values.max, grouped.values.product, numberOfJs) match
      case (5, _, _) => FiveOfAKind
      case (4, _, 4) => FiveOfAKind
      case (4, _, 1) => FiveOfAKind
      case (4, _, 0) => FourOfAKind
      case (3, 6, 3) => FiveOfAKind
      case (3, 6, 2) => FiveOfAKind
      case (3, 6, 0) => FullHouse
      case (3, _, 3) => FourOfAKind
      case (3, _, 1) => FourOfAKind
      case (3, _, 0) => ThreeOfAKind
      case (2, 4, 2) => FourOfAKind
      case (2, 4, 1) => FullHouse
      case (2, 4, 0) => TwoPair
      case (2, _, 2) => ThreeOfAKind
      case (2, _, 1) => ThreeOfAKind
      case (2, _, 0) => OnePair
      case (1, _, 1) => OnePair
      case (1, _, 0) => HighCard

object Hand:
  private def compare(first: Hand, second: Hand, extractor: Hand => Int)(using Ordering[Card]): Int =
    extractor.apply(first).compare(extractor.apply(second)) match
      case 0 => first.cards.zip(second.cards).map((cardOfThis, cardOfThat) => summon[Ordering[Card]].compare(cardOfThis, cardOfThat)).find(_ != 0).getOrElse(0)
      case value => value
  given part1Ordering(using Ordering[Card]): Ordering[Hand] with
    def compare(first: Hand, second: Hand): Int =
      -Hand.compare(first, second, _.typeOfHandPart1.ordinal)

  given part2Ordering(using Ordering[Card]): Ordering[Hand] with
    def compare(first: Hand, second: Hand): Int =
      -Hand.compare(first, second, _.typeOfHandPart2.ordinal)

case class HandWithBid(hand: Hand, bid: Long):
  override def toString: String = s"${hand.raw} ($bid) - ${hand.typeOfHandPart1} - ${hand.typeOfHandPart2}"
