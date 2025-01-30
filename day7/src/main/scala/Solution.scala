import scala.collection.immutable.HashMap

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val handsWithBids = inputLines.map:
      case s"$hand $bid" => HandWithBid(Hand(hand), bid.toLong)

    val handsOrderedForPart1 =
      import Card.part1Ordering
      import Hand.part1Ordering
      handsWithBids.sortBy(_.hand)

    val handsOrderedForPart2 =
      import Card.part2Ordering
      import Hand.part2Ordering
      handsWithBids.sortBy(_.hand)

    val List(result1, result2) = List(handsOrderedForPart1, handsOrderedForPart2).map:
      _.zipWithIndex.map:
        (handWithBid, index) => handWithBid.bid * (index + 1)
      .sum

    (s"$result1", s"$result2")

case class Card(char: Char):
  private val List(ordinalPart1, ordinalPart2) = List(Card.ordinalsPart1, Card.ordinalsPart2).map:
    ordinals => ordinals.get(char) match
      case Some(value) => value
      case None => throw Exception(s"Not valid card : $char")

object Card:
  private def asHashMap(orderedCards: List[Char]): Map[Char, Int] =
    HashMap[Char, Int](orderedCards.zipWithIndex.map:
      case (char, index) => char -> index
    : _*)
  private val letters: List[Char] = List('A', 'K', 'Q', 'J', 'T')
  private val figures: List[Char] = (9 to 2 by -1).map(_.toString.head).toList
  private val ordinalsPart1: Map[Char, Int] = asHashMap(letters ::: figures)
  private val ordinalsPart2: Map[Char, Int] = asHashMap(letters.filterNot(_ == 'J') ::: figures ::: 'J' :: Nil)

  given part1Ordering: Ordering[Card] = Ordering.by(_.ordinalPart1)

  given part2Ordering: Ordering[Card] = Ordering.by(_.ordinalPart2)

  given listOfCardOrdering(using Ordering[Card]): Ordering[List[Card]] with
    def compare(first: List[Card], second: List[Card]): Int =
      second.zip(first).map((cardOfThis, cardOfThat) => summon[Ordering[Card]].compare(cardOfThis, cardOfThat)).find(_ != 0).getOrElse(0)

enum TypeOfHand:
  case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard

import TypeOfHand.*

case class Hand(raw: String):
  private lazy val cards: List[Card] = raw.map(Card(_)).toList
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
  given part1Ordering(using Ordering[Card]): Ordering[Hand] = Ordering.by(hand => (-hand.typeOfHandPart1.ordinal, hand.cards))

  given part2Ordering(using Ordering[Card]): Ordering[Hand] = Ordering.by(hand => (-hand.typeOfHandPart2.ordinal, hand.cards))

case class HandWithBid(hand: Hand, bid: Long):
  override def toString: String = s"${hand.raw} ($bid) - ${hand.typeOfHandPart1} - ${hand.typeOfHandPart2}"
