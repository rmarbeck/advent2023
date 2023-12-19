import com.typesafe.scalalogging.Logger
import scala.io.Source
import scala.math._

import scala.collection.mutable.Map

import java.time.Duration
import java.time.Instant
// Right :-/ result is

val loggerAOC = Logger("aoc")
val loggerAOCPart1 = Logger("aoc.part1")
val loggerAOCPart2 = Logger("aoc.part2")

@main def hello: Unit =
  loggerAOC.trace("Root trace activated")
  loggerAOC.debug("Root debug activated")
  println("Launching 19-12")
  val startTime = Instant.now()

  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")
  println(s"Computing time is ${Duration.between(startTime, Instant.now()).toMillis}ms")

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq

    val (firstLines, secondLines) = lines.span(!_.isEmpty)

    val container = RuleContainer.fromString(firstLines)

    val part1 = secondLines.tail.map(Part.fromString(_)).filter(container.apply(_)).map(_.rating).sum

    val part2 = container.calcRanges().map(_.width).sum

    val (result1, result2) = (s"${part1}", s"${part2}")

    (s"${result1}", s"${result2}")



class RuleContainer(rules: Map[String, List[Rule]]):
  override def toString: String = rules.mkString
  def applyRulesToSinglePart(nameOfRule: String, part: Part): Boolean =
    val firstRuleThatMatches = rules.get(nameOfRule).get.map { currentRule =>
      currentRule.applyRule(part)
    }.find(_.isDefined)

    firstRuleThatMatches.get match
      case Some(ruleToApply) => ruleToApply match
        case Right(value) => value
        case Left(newRuleToApply) => applyRulesToSinglePart(newRuleToApply, part)
      case None => println("should not happen"); false

  def apply(part: Part): Boolean =
    applyRulesToSinglePart("in", part)


  def applyRulesToRange(nameOfRule: String, rangePart: RangePart): List[RangePart] =
    loggerAOCPart2.trace(s"applying rule $nameOfRule")
    def applyFirstRule(rules: List[Rule], rangeParts: List[RangePart]): List[RangePart] =
      rules match
        case head :: Nil =>
          val splitted = rangeParts.flatMap(_.split(head))
          splitted.filter(_.isPartiallyResolved) ::: splitted.filterNot(_.isPartiallyResolved).flatMap(_.split(head))
        case head :: tail =>
          loggerAOCPart2.trace(s"applying rule ${head} ${tail}")
          val splitted = rangeParts.flatMap(_.split(head))
          splitted.filter(_.isPartiallyResolved) ::: applyFirstRule(tail, splitted.filterNot(_.isPartiallyResolved))
        case Nil => println("should not happen"); Nil

    applyFirstRule(rules.get(nameOfRule).get, List(rangePart))

  def calcRanges() : List[RangePart] =
    import PRange.default
    calcRanges(List(RangePart(default, default, default, default, Some(Left("in")))), Nil)

  def calcRanges(notResolvedRanges: List[RangePart], resolvedRanges: List[RangePart]): List[RangePart] =
    notResolvedRanges match
      case Nil => resolvedRanges
      case head :: _ =>
        val newRanges = (head.result match
          case Some(Left(nameOfRules)) => applyRulesToRange(nameOfRules, head)
          case _ => println("should not happen"); Nil
          )
        val notResolved = newRanges.filterNot(_.isResolved)
        loggerAOCPart2.trace(s"NOT resolved is $notResolved")
        loggerAOCPart2.trace(s"=================================")
        val resolved = newRanges.filter(_.isResolved)
        loggerAOCPart2.trace(s"resolved is $resolved")
        loggerAOCPart2.trace(s"---------------------------------")
        calcRanges(notResolved ::: notResolvedRanges.tail, resolved ::: resolvedRanges)


object RuleContainer:
  def fromString(input: Seq[String]) =
    def getRules(rules: String): List[Rule] =
      rules.split(",").map(Rule.fromString(_)).toList
    val rulesToBuild = Map[String, List[Rule]]()
    input.foreach :
      case s"$name{$rules}" => rulesToBuild += name -> getRules(rules)
    RuleContainer(rulesToBuild)


case class Rule(letter: Option[Char], operation: Option[Char], value: Option[Long], result: Either[String, Boolean]):
  def applyRule(part: Part): Option[Either[String, Boolean]] =
    (letter, operation, value, result) match
      case (Some(letterValue), Some(operationValue), Some(valueValue), resultValue) => operationValue match
        case '>' if part.valueOf(letterValue) > valueValue => Some(result)
        case '<' if part.valueOf(letterValue) < valueValue => Some(result)
        case _ => None
      case _ => Some(result)

object Rule:
  def fromString(input: String) =
    def mapAction(value: String): Either[String, Boolean] =
      value match
        case "A" => Right(true)
        case "R" => Right(false)
        case value => Left(value)
    input match
      case s"${start}:${action}" => Rule(Some(start.charAt(0)), Some(start.charAt(1)), Some(start.drop(2).toLong), mapAction(action))
      case s"${action}" => Rule(None, None, None, mapAction(action))

case class PRange(start: Int, end: Int):
  def size: Long = end - start + 1

  def applyRule(operation: Char, value: Int): PRange =
    operation match
      case '>' => this.copy(start = max(start, value +1))
      case _ => this.copy(end = min(end, value - 1))

  def applyOppositeRule(operation: Char, value: Int): PRange =
    operation match
      case '>' => this.copy(end = min(end, value))
      case _ => this.copy(start = max(start, value))

  override def toString: String = s"[${start}-${end}]"

object PRange:
  def default:PRange = PRange(1, 4000)

case class RangePart(x: PRange, m: PRange, a: PRange, s: PRange, result: Option[Either[String, Boolean]]):
  override def toString: String = s"{$x $m $a $s} ${result.get.getOrElse(false)} = $width"
  def width: Long =
    isAccepted match
      case false => 0
      case true =>
        val result:Long = x.size * m.size * a.size * s.size
        loggerAOCPart2.trace(s"$result")
        result

  def isPartiallyResolved = result.isDefined
  def isResolved = result.map(_.isRight).getOrElse(false)
  def isAccepted = isResolved match
    case false => false
    case true => result.get.getOrElse(false)

  def addAConstraint(letter: Char, operation: Char, value: Int, result: Option[Either[String, Boolean]], toApply: PRange => PRange): RangePart =
    letter match
      case 'x' => this.copy(x = toApply(x), result = result)
      case 'm' => this.copy(m = toApply(m), result = result)
      case 'a' => this.copy(a = toApply(a), result = result)
      case 's' => this.copy(s = toApply(s), result = result)

  def addOppositeConstraint(letter: Char, operation: Char, value: Int, result: Option[Either[String, Boolean]]): RangePart =
    addAConstraint(letter, operation, value, result, prange => prange.applyOppositeRule(operation, value))

  def addConstraint(letter: Char, operation: Char, value: Int, result: Option[Either[String, Boolean]]): RangePart =
    addAConstraint(letter, operation, value, result, prange => prange.applyRule(operation, value))

  def split(rule: Rule): List[RangePart] =
    loggerAOCPart2.trace(s"splitting on rule $rule for $this")
    rule match
      case Rule(Some(letterValue), Some(operationValue), Some(valueValue), resultValue) =>
        List(this.addConstraint(letterValue, operationValue, valueValue.toInt, Some(resultValue)), this.addOppositeConstraint(letterValue, operationValue, valueValue.toInt, None))
      case Rule(_, _, _, resultValue) => List(this.copy(result = Some(resultValue)))

case class Part(x: Long, m: Long, a: Long, s: Long):
  def rating: Long = x + m + a + s
  def valueOf(letter: Char): Long =
    letter match
      case 'x' => x
      case 'm' => m
      case 'a' => a
      case 's' => s

object Part:
  def fromString(input: String) =
    input match
      case s"{x=${valueX},m=${valueM},a=${valueA},s=${valueS}}" => Part(valueX.toLong, valueM.toLong, valueA.toLong, valueS.toLong)