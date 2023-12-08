import scala.io.Source
import scala.math._

// Right :-/ result is

@main def hello: Unit =
  println("Launching 5-12")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq
    val hands = lines.map(HandBidded.fromLine(_))

    //println(s"count is ${hands.sorted.reverse.zipWithIndex.map((value, index) => ((index+1) * value.bid).toLong)}")
    //hands.sorted.reverse.foreach(println)
    //hands.sortBy(_.handAsStringWithJoker).reverse.foreach(println)
    //println(hands.sorted.zip(hands.sortBy(_.handAsString)).map((first, second) => first == second).filter(_ == false))
    //val result1, result2 = hands.sorted.reverse.zipWithIndex.map((value, index) => ((index+1) * value.bid).toLong).sum

    val part1Sorting = hands.sorted.reverse
    val part2Sorting = hands.sortBy(_.handAsStringWithJoker).reverse

    val (result1, result2) = List(part1Sorting, part2Sorting).map(_.zipWithIndex.foldLeft(0l) { (acc, value) =>
      val result = acc + ((value._2 + 1) * value._1.bid)
      //println(s"$value \t<= ${value} * index = ${result}")
      result
    }) match
      case List(first, second) => (first, second)
      case _ => ("", "")



    (s"${result1}", s"${result2}")

case class HandBidded(hand: String, bid: Int) extends Ordered[HandBidded]:
  def maxInSubHand(subHand: String): (Char, String) =
    subHand.groupBy(value => value).toSeq.maxBy((key, value) => value.length)
  def maxNumberInSubHand(subHand: String): Int =
    maxInSubHand(subHand)._2.length
  def maxCardByOccurenceInSubHand(subHand: String): Char =
    maxInSubHand(subHand)._1
  override def compare(toThisOne: HandBidded): Int =
    rank-toThisOne.rank match
      case value if value < 0 => -1
      case value if value > 0 => 1
      case 0 => {
        //println(s"comparing ${this} and ${toThisOne} => ${hand.zip(toThisOne.hand).map((cardInThis, otherCard) => valueOfCard(cardInThis) - valueOfCard(otherCard))}")
        val result = hand.zip(toThisOne.hand).map((cardInThis, otherCard) => valueOfCard(cardInThis) - valueOfCard(otherCard)).filter(_ != 0).headOption.getOrElse(0) match
          case value if value < 0 => -1
          case value if value > 0 => 1
          case 0 => println("should not") ; 0
        //println(s"result is ${result}")
        result
      }

  def rank: Int =
    //println(s"===> ${hand.groupBy(value => value).toSeq.maxBy((key, value) => value.length)}")
    hand match
      case _ if fiveOfAkind => 0
      case _ if fourOfAkind => 1
      case _ if fullHouse => 2
      case _ if threeOfAkind => 3
      case _ if twoPairs => 4
      case _ if onePair => 5
      case _ => 6

  def rankWithJoker: Int =
  //println(s"===> ${hand.groupBy(value => value).toSeq.maxBy((key, value) => value.length)}")
    hand.filterNot(_ == 'J') match
      case handWithoutJoker if handWithoutJoker.length <= 1 => 0 // => becomes a fiveOfAKind
      case handWithoutJoker if handWithoutJoker.length == 2 => 1 // => becomes a fourOfAKind
      case handWithoutJoker if handWithoutJoker.length == 3 =>
        maxNumberInSubHand(handWithoutJoker) match
          case 3 => 0 // => becomes a fiveOfAKind
          case 2 => 1 // => becomes a fourOfAKind
          case _ => 3 // => becomes a fourOfAKind
      case handWithoutJoker if handWithoutJoker.length == 4 =>
        maxNumberInSubHand (handWithoutJoker) match
          case 4 => 0 // => becomes a fiveOfAKind
          case 3 => 1 // => becomes a fourOfAKind
          case 2 => this.rank match
            case 4 => 2 // => becomes a fullHouse
            case _ => 3 // => becomes a threeOfAKind
          case _ => 5 // => becomes a pair
      case _ => this.rank

  def ofAKind(number: Int): Boolean =
    maxNumberInSubHand(hand) == number
  def fiveOfAkind: Boolean =
    ofAKind(5)
  def fourOfAkind: Boolean =
    ofAKind(4)
  def threeOfAkind: Boolean =
    ofAKind(3)
  def fullHouse: Boolean =
    if (threeOfAkind)
      val foundChar = maxCardByOccurenceInSubHand(hand)
      //println(s"$hand => checking ${hand.filterNot(_ == foundChar)}")
      maxNumberInSubHand(hand.filterNot(_ == foundChar)) == 2
    else
      false
  def twoPairs: Boolean =
    if (ofAKind(2))
      val foundChar = maxCardByOccurenceInSubHand(hand)
      maxNumberInSubHand(hand.filterNot(_ == foundChar)) == 2
    else
      false
  def onePair: Boolean =
    ofAKind(2) && !twoPairs
  def valueOfHighest: Int =
    hand.map(valueOfCard(_)).min
  def valueOfCard(card: Char): Int =
    card match
      case 'A' => 1
      case 'K' => 2
      case 'Q' => 3
      case 'J' => 4
      case 'T' => 5
      case intVal => 6 + (9 - intVal.asDigit)

  def rankExplained: String =
    rank match
      case 0 => "\t5 of a kind"
      case 1 => "\t4 of a kind"
      case 2 => "\tfull house"
      case 3 => "\t3 of a kind"
      case 4 => "\t2 pairs"
      case 5 => "\t1 pair"
      case 6 => s"	highest cars A"
      case 7 => s"	highest cars K"
      case 8 => s"	highest cars Q"
      case 9 => s"	highest cars J"
      case 10 => s"	highest cars T"
      case value => s"	highest cars ${20 - value}"

  def cardAsChar(card: Char): String =
    val alphabetFromF = (0 to 20).map(_+'f').map(_.toChar).mkString
    (card match
      case 'A' => 'a'
      case 'K' => 'b'
      case 'Q' => 'c'
      case 'J' => 'd'
      case 'T' => 'e'
      case intVal => alphabetFromF.charAt(9 - intVal.asDigit)
      ).toString

  def handAsChars: String =
    hand.map(cardAsChar(_)).mkString

  def handWithJokerAsChars: String =
    hand.map(cardAsChar(_).replace("d", "z")).mkString

  def rankAsChar: String  =
    val alphabet = (0 to 25).map(_+'a').map(_.toChar).mkString
    alphabet.charAt(rank).toString

  def rankWithJokerAsChar: String =
    val alphabet = (0 to 25).map(_ + 'a').map(_.toChar).mkString
    alphabet.charAt(rankWithJoker).toString

  def handAsString: String =
    s"${rankAsChar}${handAsChars}"

  def handAsStringWithJoker: String =
    s"${rankWithJokerAsChar}${handWithJokerAsChars}"

  override def toString: String =
    s"$hand\t = ${handAsString}\t${handAsStringWithJoker}\t- bid is '$bid'\t- ranking is $rank <=> $rankExplained"

object HandBidded:
  def fromLine(line :String): HandBidded =
    val values = line.replaceAll(" ", ",").span(_ != ',')
    HandBidded(values._1.trim, values._2.tail.trim.toInt)

