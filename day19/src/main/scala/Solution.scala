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

    val resultPart1 = enteringRuleHolder match
      case Some(enterBy) => parts.filter(testPart(_, enterBy)).map(_.total).sum
      case None => throw Exception("Unable to find start")

    val result1 = s"$resultPart1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

@tailrec
def testPart(part: Part, ruleHolder: RuleHolder)(using Rules): Boolean =
  ruleHolder.filter(part) match
    case Left(nextHolderName) => testPart(part, summon[Rules](nextHolderName))
    case Right(value) => value

type Rules = Map[String, RuleHolder]
type RuleHolderName = String
type Alternative = Either[RuleHolderName, Boolean]

case class Part(x: Int, m: Int, a: Int, s: Int):
  def total: Int = x + m + a +s

  def byKey(name: Char): Int =
    name match
      case 'x' => x
      case 'm' => m
      case 'a' => a
      case 's' => s
      case _ => throw Exception("Impossible")

object Part:
  def from(asList: List[Int]): Part =
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
  def filter(part: Part): Alternative =
    rules match
      case Nil => default
      case head :: tail => head.filter(part) match
        case None => this.copy(rules = tail).filter(part)
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
  def filter(part: Part): Option[Alternative] =
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