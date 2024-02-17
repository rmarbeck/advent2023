
type Length = Long
type Start = Long
type Destination = Long

case class ClosedMapping(start: Long, end: Long, drift: Long) extends MergeableRange[Long]:
  override type Same = ClosedMapping
  override def compareStarts(other: Same): Int = this.start.compareTo(other.start)
  override def compareEnds(other: Same): Int = this.end.compareTo(other.end)
  override def compareStartAndEndOf(other: Same): Int = this.start.compareTo(other.end)
  override def compareEndAndStartOf(other: Same): Int = this.end.compareTo(other.start)

  def map(value:Long): Option[Long] = Option.when(value >= start && value <= end)(value + drift)

  def drifted = ClosedMapping(startAfterDrifting, endAfterDrifting, 0)

  def startAfterDrifting = start + drift
  def endAfterDrifting = end + drift

  def touchAfterDrifting(other: ClosedMapping): Boolean =
    drifted overlap other

  def mergeFrom(otherPreviousMapping: ClosedMapping): List[ClosedMapping] =
    val previousMappingDrifted = otherPreviousMapping.drifted
    val before =
      previousMappingDrifted.startsStrictlyBefore(this) match
        case true => Some(otherPreviousMapping.copy(end = otherPreviousMapping.start + this.start - previousMappingDrifted.start - 1))
        case false => None

    val after = previousMappingDrifted.endsStrictlyAfter(this) match
      case true => Some(otherPreviousMapping.copy(start = this.end - otherPreviousMapping.drift + 1))
      case false => None

    val inside = (this.startsBefore(previousMappingDrifted), this.endsStrictlyBefore(previousMappingDrifted)) match
      case (true, true) =>
        println("1")
        List(
          this.copy(end = otherPreviousMapping.start - 1),
          ClosedMapping(otherPreviousMapping.start, this.end - otherPreviousMapping.drift,drift = this.drift + otherPreviousMapping.drift))
      case (true, false) =>
        println("2")
        List(
          this.copy(end = otherPreviousMapping.start - 1),
          otherPreviousMapping.copy(drift = otherPreviousMapping.drift + this.drift),
          this.copy(start = otherPreviousMapping.end + 1))
      case (false, false) =>
        println("3")
        List(
          ClosedMapping(otherPreviousMapping.start + this.start - previousMappingDrifted.start, previousMappingDrifted.end,drift = this.drift + otherPreviousMapping.drift),
          this.copy(start = previousMappingDrifted.end + 1)
        )
      case (false, true) =>
        println("4")
        List(ClosedMapping(otherPreviousMapping.start - (previousMappingDrifted.start - this.start), this.end - otherPreviousMapping.drift, this.drift + otherPreviousMapping.drift))

    println(s" before : ${before.isDefined},  after : ${after.isDefined}")

    inside ::: List(before, after).flatten


object ClosedMapping:
  def from(destination: Destination, start: Start, length: Length) =
    ClosedMapping(start: Long, start + length - 1l : Long, destination - start : Long)

  def from(mapping: Mapping) =
    ClosedMapping(mapping.destinationStart: Destination, mapping.sourceStart: Start, mapping.length: Length)

case class MappingFunction(name: String, mappings: List[ClosedMapping]):
  def smallestFrom2(start: Long, end: Long): Long =
    mappings.filter:
      case closedMapping if closedMapping.start >= start && closedMapping.start <= end => true
      case _ => false
    .flatMap:
      case closedMapping => closedMapping.map(closedMapping.start)
    .minOption.getOrElse(start)

  def smallestFrom(start: Long, end: Long): Long =
    val startIsAlreadyTakenIntoAccount = mappings.exists:
      case closedMapping => closedMapping.start <= start && closedMapping.end >= start
    val inDefinedMappings = mappings.flatMap:
      case closedMapping if closedMapping.start <= start && closedMapping.end >= start => closedMapping.map(start)
      case closedMapping if closedMapping.start >= start && closedMapping.start <= end => closedMapping.map(closedMapping.start)
      case _ => None
    .minOption.getOrElse(start)

    startIsAlreadyTakenIntoAccount match
      case false => math.min(start, inDefinedMappings)
      case true => inDefinedMappings

  def digest(nextLevelMappingFunction: MappingFunction): MappingFunction =
    val nextLevelMapping = nextLevelMappingFunction.mappings
    val (originalMappingsModified, originalMappingsUnModified) = mappings.partition(initialMapping => nextLevelMapping.exists(initialMapping touchAfterDrifting _))
    val (nextLevelMappingsModified, nextLevelMappingsUnModified) = nextLevelMapping.partition(nextMapping => mappings.exists(_ touchAfterDrifting nextMapping))

    val modifiedMappings = (for
      currentNextMapping <- nextLevelMappingsModified
      currentInitialMapping <- originalMappingsModified
      if currentInitialMapping touchAfterDrifting currentNextMapping
    yield
      currentNextMapping mergeFrom currentInitialMapping
      ).flatten

    MappingFunction(name + "+" + nextLevelMappingFunction.name, originalMappingsUnModified ::: nextLevelMappingsUnModified ::: modifiedMappings)

object MappingFunction:
  def fromMapper(mapper: Mapper) : MappingFunction =
    val closedMappings = mapper.mappings.map(ClosedMapping.from(_))
    MappingFunction(mapper.name, closedMappings)

trait MergeableRange[T](using ordering: Ordering[T]):
  type Same

  def startsBefore(other: Same): Boolean = compareStarts(other) <= 0
  def startsAfter(other: Same): Boolean = compareStarts(other) >= 0
  def startsStrictlyBefore(other: Same): Boolean = compareStarts(other) < 0
  def startsStrictlyAfter(other: Same): Boolean = compareStarts(other) > 0
  def startsTogether(other: Same): Boolean = compareStarts(other) == 0
  def endsBefore(other: Same): Boolean = compareEnds(other) <= 0
  def endsAfter(other: Same): Boolean = compareEnds(other) >= 0
  def endsStrictlyBefore(other: Same): Boolean = compareEnds(other) < 0
  def endsStrictlyAfter(other: Same): Boolean = compareEnds(other) > 0
  def endsTogether(other: Same): Boolean = compareEnds(other) == 0

  def endsStrictlyBeforeStartOf(other: Same): Boolean = compareEndAndStartOf(other) < 0
  def startsStrictlyAfterEndOf(other: Same): Boolean = compareStartAndEndOf(other) > 0

  def overlap(other: Same): Boolean = ! (startsStrictlyBefore(other) && endsStrictlyBefore(other) || startsStrictlyAfter(other) && endsStrictlyAfter(other))
  def includes(other: Same): Boolean = overlap(other) && startsBefore(other) && endsAfter(other)
  def isIncludedIn(other: Same): Boolean = overlap(other) && startsAfter(other) && endsBefore(other)

  def compareStarts(other: Same): Int
  def compareEnds(other: Same): Int
  def compareStartAndEndOf(other: Same): Int
  def compareEndAndStartOf(other: Same): Int