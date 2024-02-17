import MultiLayerSolver.merge

case class MultiLayerSolver(layers: List[SingleLayerRangeSet]):
  private val mergedLayer = merge(layers)
  def solve(value: Long): Long = solveRange(value, value)
  def solveRange(startIncluded: Long, endIncluded: Long): Long = mergedLayer.solveRange(startIncluded, endIncluded)

object MultiLayerSolver:
  private def merge(layers: List[SingleLayerRangeSet]): SingleLayerRangeSet =
    layers.tail.foldLeft(layers.head):
      case (acc, newLayer) => mergeIn(acc, newLayer)

  private def mergeIn(initialLayer: SingleLayerRangeSet, nextLevelLayer: SingleLayerRangeSet): SingleLayerRangeSet =
    val initialLayerSlices =
      initialLayer.sortedRanges match
        case initialRange @ head :: tail if head.start == 0 => initialRange
        case initialRange @ head :: tail => RangeMap(0, head.start - 1, 0) +: initialRange
        case _ => throw Exception("Should not happen")

    val newRangeMaps = initialLayerSlices.flatMap(nextLevelLayer.mergeWithTopLevelRange(_))
    SingleLayerRangeSet(initialLayer.name+"-"+nextLevelLayer.name, newRangeMaps)

case class SingleLayerRangeSet(name: String, ranges: List[RangeMap]):
  import scala.collection.immutable.SortedMap
  val sortedRanges = ranges.sortBy(_.start)
  require(SingleLayerRangeSet.rangesDoNotOverlap(sortedRanges))
  val limits: Map[Long, Long] = SortedMap(sortedRanges.sliding(2, 1).flatMap:
    case List(first, second) if second.start == first.endIncluded + 1 => List(first.start -> first.drift, second.start -> second.drift, second.endIncluded + 1 -> 0l)
    case List(first, second) => List(first.start -> first.drift, first.endIncluded + 1 -> 0l, second.start -> second.drift, second.endIncluded + 1 -> 0l)
    case _ => Nil
  .toList: _*)

  def limitsDriftedTo(drift: Long): Map[Long, Long] = SortedMap(limits.map((key, value) => (key - drift) -> value).toList: _*)

  def solve(value: Long): Long = solveRange(value, value)

  def solveRange(startIncluded: Long, endIncluded: Long): Long =
    val firstLimit = startIncluded < limits.keys.min match
      case true => Some(startIncluded)
      case false => limits.filterNot((key, _) => key > startIncluded).map((key, value) => math.max(key, startIncluded) + value).lastOption

    val insideLimits = limits.filter((key, _) => key > startIncluded && key <= endIncluded).map((key, value) => key + value).toList

    (firstLimit match
      case Some(value) => insideLimits :+ value
      case None => insideLimits
      ).min

  def mergeWithTopLevelRange(topLevelRange: RangeMap): List[RangeMap] =
    def manageNextStartsBefore(overlapingHead: Option[(Long, Long)], firstInRange: Option[(Long, Long)]): List[RangeMap] =
      (overlapingHead, firstInRange) match
        case (Some(ovelapping), Some(inRange)) => List(RangeMap(topLevelRange.start, inRange._1 - 1, ovelapping._2 + topLevelRange.drift))
        case (Some(ovelapping), None) => List(RangeMap(topLevelRange.start, topLevelRange.endIncluded, ovelapping._2 + topLevelRange.drift))
        case _ => Nil

    def manageInRange(inRange: Map[Long, Long], headIsEmpty: Boolean): List[RangeMap] =
      val newLimits: Map[Long, Long] = inRange.map((key, value) => key -> (value + topLevelRange.drift))
      val ranges = SingleLayerRangeSet.toListOfRange(newLimits, topLevelRange.endIncluded + 1)
      (headIsEmpty, topLevelRange.start < inRange.keys.toList.sorted.minOption.getOrElse(topLevelRange.start)) match
        case (true, true) =>
          (RangeMap(topLevelRange.start, inRange.keys.toList.sorted.min - 1, topLevelRange.drift) +: ranges).sortBy(_.start)
        case _ =>
          ranges

    val headMatching = limitsDriftedTo(topLevelRange.drift).filterNot((key, _) => key > topLevelRange.endIncluded)
    val (includingFirst, others) = headMatching.span((key, _) => key < topLevelRange.start)


    val newHead = manageNextStartsBefore(includingFirst.lastOption, others.headOption)


    val newBody = manageInRange(others, newHead.isEmpty)

    val output =
      newHead ::: newBody match
        case Nil => List(topLevelRange)
        case value => value

    output

object SingleLayerRangeSet:
  def from(mapper: Mapper): SingleLayerRangeSet =
    SingleLayerRangeSet(mapper.name, mapper.mappings.map(RangeMap.from(_)))

  def rangesDoNotOverlap(sortedRanges: List[RangeMap]): Boolean =
    val overlapingDetected =
      sortedRanges.sliding(2, 1).exists:
        case List(first, second) => second.start <= first.endIncluded
        case _ => false
    !overlapingDetected

  def toListOfRange(limits: Map[Long, Long], end: Long): List[RangeMap] =
    import scala.collection.immutable.SortedMap
    val extendedLimits = SortedMap((limits + (end -> 0l)).toList: _*)
    extendedLimits.keys.toList.sliding(2, 1).flatMap:
      case List(first, second) => Some(RangeMap(first, second - 1 , extendedLimits.get(first).get))
      case _ => None
    .toList

case class RangeMap(start: Long, endIncluded: Long, drift: Long)

object RangeMap:
  def from(start: Long, length: Long, destinationOfStart: Long): RangeMap =
    RangeMap(start, start + length - 1, destinationOfStart - start)

  def from(mapping: Mapping): RangeMap =
    RangeMap.from(start = mapping.sourceStart, length = mapping.length, destinationOfStart = mapping.destinationStart)