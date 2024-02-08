import scala.reflect.ClassTag

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val games = inputLines.map:
      case s"Game $gameNumber: $games" => Game(gameNumber.toInt, Draw.fromListOfRawResults(games))

    val bag = Bag(reds = 12, blues = 14, greens = 13)

    val resultPart1 = games.filter(_.isBagContentPossible(bag)).map(_.number).sum
    val resultPart2 = games.map(_.minimalBagPossible.power).sum

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Bag(reds: Int, blues: Int, greens: Int):
  def power: Long = reds * blues * greens

object Bag:
  given FromColors[Bag] with
    override def fromColors(reds: Int, blues: Int, greens: Int): Bag = Bag(reds, blues, greens)

case class Draw(reds: Int, blues: Int, greens: Int):
  def colors: List[Int] = List(reds, blues, greens)
  def cannotGoOutFromBag(bag: Bag): Boolean =
    bag.reds < reds || bag.blues < blues || bag.greens < greens

object Draw:
  given FromColors[Draw] with
    override def fromColors(reds: Int, blues: Int, greens: Int): Draw = Draw(reds, blues, greens)

  def fromListOfRawResults(resultsAsString: String): Seq[Draw] =
    def fromRawResult(singleResultAsString: String): Draw =
      val colors = singleResultAsString.split(", ").map:
        case s"$reds red" => (reds.toInt, 0, 0)
        case s"$blues blue" => (0, blues.toInt, 0)
        case s"$greens green" => (0, 0, greens.toInt)
        case value => throw Exception(s"Not supported format : $value")

      val singleColors = colors.unzip3.toList.map(_.sum)
      fromColors[Draw](singleColors)

    resultsAsString.split("; ").map(fromRawResult(_)).toSeq

case class Game(number: Int, draws: Seq[Draw]):
  def minimalBagPossible: Bag =
    val colorsOfBag = draws.foldLeft(List(0, 0, 0)):
      case (acc, draw) => acc.zip(draw.colors).map(math.max(_, _))

    fromColors[Bag](colorsOfBag)

  def isBagContentPossible(bag: Bag): Boolean =
    ! draws.exists(_.cannotGoOutFromBag(bag))

/**
 *
 * Type class to create from list of Ints an instance that needs all three colors
 *
 */
trait FromColors[T]:
  def fromColors(reds: Int, blues: Int, greens: Int): T

def fromColors[T : FromColors](colors: List[Int]): T =
  colors match
    case reds :: blues :: greens :: Nil => summon[FromColors[T]].fromColors(reds, blues, greens)
    case value => throw Exception(s"Cannot create from List : $value")