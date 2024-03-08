import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (ruleHoldersInput, partsInput) = inputLines.span(_.nonEmpty)

    val ruleHolders = ruleHoldersInput.map(RuleHolder.fromString)
    val parts = partsInput.tail.map(Part.fromString)

    val rules = HashMap(ruleHolders.map(current => current.name -> current):_*)

    given Rules = rules
    val enteringRuleHolder = rules.get("in")

    val (resultPart1, resultPart2) = enteringRuleHolder match
      case Some(enterBy) =>
        (parts.filter(testPart(_, enterBy)).map(_.total).sum, testPartRange(PartRange.default, enterBy).map(_.combinations).sum)
      case None => throw Exception("Unable to find start")

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def testPart(part: Part, ruleHolder: RuleHolder)(using Rules): Boolean =
  ruleHolder.test(part) match
    case Left(nextHolderName) => testPart(part, summon[Rules](nextHolderName))
    case Right(value) => value

def testPartRange(partRange: PartRange, ruleHolder: RuleHolder)(using Rules): List[PartRange] = ruleHolder.test(partRange)

type Rules = Map[String, RuleHolder]
type RuleHolderName = String
type Alternative = Either[RuleHolderName, Boolean]

case class Part(x: Int, m: Int, a: Int, s: Int):
  def total: Int = x + m + a + s

  def byKey(name: Char): Int =
    name match
      case 'x' => x
      case 'm' => m
      case 'a' => a
      case 's' => s
      case _ => throw Exception("Impossible")

object Part:
  private def from(asList: List[Int]): Part =
    asList match
      case x :: m :: a :: s :: Nil => Part(x, m, a, s)
      case _ => throw Exception("Not supported")
  def fromString(raw: String): Part =
    raw match
      case s"{x=$x,m=$m,a=$a,s=$s}" =>
        List(x, m, a, s).flatMap(_.toIntOption) match
          case list if list.length == 4 => from(list)
          case _ => throw Exception("At least one data is not an Int")
      case _ => throw Exception("Not supported format")

case class PartRange(x: (Int, Int), m: (Int, Int), a: (Int, Int), s: (Int, Int)):
  def combinations: Long = List(x, m, a, s).map(PartRange.size).product

  private type Splitter = (Int, (Int, Int)) => List[(Int, Int)]

  private def splitInclusive(limit: Int, of: (Int, Int)): List[(Int, Int)] = split(limit, of)
  private def splitExclusive(limit: Int, of: (Int, Int)): List[(Int, Int)] = split(limit - 1, of)

  private def split(limit: Int, of: (Int, Int)): List[(Int, Int)] =
    (limit, of._1, of._2) match
      case (lim, min, max) if lim < min => List((min, max))
      case (lim, min, max) if lim > max => List((min, max))
      case (lim, min, max) => List((min, lim), (lim+1, max))

  def splitInclusive(limit: Int, key: Char): List[PartRange] = split(limit, key, splitInclusive)
  def splitExclusive(limit: Int, key: Char): List[PartRange] = split(limit, key, splitExclusive)

  private def split(limit: Int, key: Char, splitter: Splitter): List[PartRange] =
    key match
      case 'x' => splitter.apply(limit, x).map(value => this.copy(x = value))
      case 'm' => splitter.apply(limit, m).map(value => this.copy(m = value))
      case 'a' => splitter.apply(limit, a).map(value => this.copy(a = value))
      case 's' => splitter.apply(limit, s).map(value => this.copy(s = value))

  def byKey(name: Char): (Int, Int) =
    name match
      case 'x' => x
      case 'm' => m
      case 'a' => a
      case 's' => s
      case _ => throw Exception("Impossible")

object PartRange:
  private def size(range: (Int, Int)): Long = range(1) - range(0) + 1

  lazy val default: PartRange =
    val defaultRange = (1, 4000)
    PartRange(defaultRange, defaultRange, defaultRange, defaultRange)

enum Operation:
  override def toString: String =
    this match
      case Greater => ">"
      case Lower => "<"
  case Greater, Lower
  def test(value: Int, limit: Int): Boolean =
    this match
      case Greater => value > limit
      case Lower => value < limit

export Operation.*

case class RuleHolder(name: String, rules: List[Rule], default: Alternative):
  private def manage(alternative: Alternative, matchingRange: PartRange)(using Rules): List[PartRange] =
    alternative match
      case Right(true) => List(matchingRange)
      case Left(ruleHolderName) => summon[Rules](ruleHolderName).test(matchingRange)
      case _ => Nil

  def test(partRange: PartRange)(using Rules): List[PartRange] =
    rules match
      case Nil => manage(default, partRange)
      case head :: tail =>
        val (matchingPart, unMatchingPart) = head.test(partRange)
        val resultFromUnMatching = unMatchingPart.map(this.copy(rules = tail).test(_)).getOrElse(Nil)

        val resultFromMatching =
          matchingPart match
            case Some((matchingRange, alternative)) => manage(alternative, matchingRange)
            case _ => Nil

        resultFromUnMatching ::: resultFromMatching

  def test(part: Part): Alternative =
    rules match
      case Nil => default
      case head :: tail => head.test(part) match
        case None => this.copy(rules = tail).test(part)
        case Some(alternative) => alternative

object RuleHolder:
  def fromString(rawString: String): RuleHolder =
    rawString match
      case s"$name{$rawRulesAsString}" =>
        val rawRules: Array[Rule | Alternative] = rawRulesAsString.split(",").map:
          case s"$key>$limit:$destination" => Rule(key.head, Greater, limit.toInt, destination.asDestination)
          case s"$key<$limit:$destination" => Rule(key.head, Lower, limit.toInt, destination.asDestination)
          case s"$destination" => destination.asDestination

        val (rules, Some(default)) =
          rawRules.foldLeft((List[Rule](), None : Option[Alternative])):
            case (acc, rule: Rule) => (rule +: acc._1, acc._2)
            case (acc, default: Alternative) => (acc._1, Some(default))
        : @unchecked

        RuleHolder(name, rules.reverse, default)

case class Rule(key: Char, op: Operation, limit: Int, destination: Alternative):
  def test(partRange: PartRange): (Option[(PartRange, Alternative)], Option[PartRange]) =
    val (minimum ,maximum) = partRange.byKey(key)
    (minimum, maximum, op) match
      case (min, max, Greater) if max <= limit => (None, Some(partRange))
      case (min, max, Lower) if min >= limit => (None, Some(partRange))
      case (min, max, Greater) if min > limit => (Some((partRange, destination)), None)
      case (min, max, Lower) if max < limit => (Some((partRange, destination)), None)
      case (min, max, Greater) =>
        val List(rangeBeforeLimit, rangeAfterLimit) = partRange.splitInclusive(limit, key)
        (Some((rangeAfterLimit, destination)), Some(rangeBeforeLimit))
      case (min, max, Lower) =>
        val List(rangeBeforeLimit, rangeAfterLimit) = partRange.splitExclusive(limit, key)
        (Some((rangeBeforeLimit, destination)), Some(rangeAfterLimit))

  def test(part: Part): Option[Alternative] =
    part.byKey(key) match
      case value if op.test(value, limit) => Some(destination)
      case _ => None

  override def toString: String = s"$key $op $limit => ${destination.asString}"

extension (alternative: Either[RuleHolderName, Boolean])
  private def acceptOrReject(boolean: Boolean): String =
    boolean match
      case true => "Accept"
      case false => "Reject"

  def asString: String = alternative.fold(name => s"[${name}]", acceptOrReject)

extension (s: String)
  def asDestination: Alternative =
    s match
      case "A" => Right(true)
      case "R" => Right(false)
      case value => Left(value)