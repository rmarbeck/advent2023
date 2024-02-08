object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val games = inputLines.map:
      case s"Game $gameNumber: $games" => Game(gameNumber.toInt, Draw.fromListOfRawResults(games))

    val bag = Bag(12, 14, 13)

    val resultPart1 = games.filter(_.isBagContentPossible(bag)).map(_.number).sum
    val resultPart2 = games.map(_.minimalBagPossible.power).sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Bag(reds: Int, blues: Int, greens: Int):
  def power: Long = reds * blues * greens

object Bag:
  def fromColors(colors: List[Int]): Bag =
    colors match
      case reds :: blues :: greens :: Nil => Bag(reds, blues, greens)
      case value => throw Exception(s"Cannot create Bag from List : $value")

case class Draw(reds: Int, blues: Int, greens: Int):
  def colors: List[Int] = List(reds, blues, greens)
  def cannotGoOutFromBag(bag: Bag): Boolean =
    bag.reds < reds || bag.blues < blues || bag.greens < greens

object Draw:
  def fromListOfRawResults(resultsAsString: String): Seq[Draw] =
    def fromRawResult(singleResultAsString: String): Draw =
      val colors = singleResultAsString.split(", ").map:
        case s"$reds red" => (reds.toInt, 0, 0)
        case s"$blues blue" => (0, blues.toInt, 0)
        case s"$greens green" => (0, 0, greens.toInt)
        case value => throw Exception(s"Not supported format : $value")

      val singleColors = colors.unzip3.toList.map(_.sum)
      Draw(singleColors(0), singleColors(1), singleColors(2))

    resultsAsString.split("; ").map(fromRawResult(_)).toSeq

case class Game(number: Int, draws: Seq[Draw]):
  def minimalBagPossible: Bag =
    val colorsOfBag = draws.foldLeft(List(0, 0, 0)):
      case (acc, draw) => acc.zip(draw.colors).map(math.max(_, _))

    Bag.fromColors(colorsOfBag)

  def isBagContentPossible(bag: Bag): Boolean =
    ! draws.exists(_.cannotGoOutFromBag(bag))