import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
import javax.print.attribute.standard.MediaSize.Other
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.seqIsParallelizable
import collection.parallel.CollectionConverters.VectorIsParallelizable

// Right :-/ result is

@main def hello: Unit =
  println("Launching 5-12")
  List[() => (String, String)]( () => Solver.solveTest, () => Solver.solve).foreach: f =>
    val (score1, score2) = f.apply()
    println(s"1 : ${score1}")
    println(s"2 : ${score2}")
    println(s"----------------")
  println("Done")

val startTime = Instant.now()

object Solver:
  def solveTest: (String, String) =
    solver("test.txt")
  def solve: (String, String) =
    solver("data.txt")
  private def solver(fileName: String): (String, String) =
    val bufferedSource = Source.fromFile("./src/main/resources/" + fileName)
    val lines = bufferedSource.getLines().toSeq
    val managedLines = lines.map:
        case s"$start map:" => s"${start}_map:"
        case s"seeds: $values" => s"seeds:${values}"
        case s"seed${name} map: $values" => s"seed${name} map:$values"
        case line if !line.isEmpty => line + ";"
        case emptyLine => "|"
    .mkString("").replace(" ", ",").split('|').toSeq
    val filteringMaps = (managedLines.filter(_.contains("map")).map:
      case s"${name}_map:${values}" => SolvingMaps(name, values.split(';').toIndexedSeq)
      ).toSeq

    val seeds: Seq[Long] = managedLines.head.drop(6).split(',').map(_.toLong).toSeq
    val computingStartTime = Instant.now()
    //val (result1, result2) = IdiomaticSolution.solve(seeds, filteringMaps)
    val (result1, result2) = DumbButFastSolution.solve(seeds, filteringMaps)
    println(s"Computing time is ${Duration.between(startTime, Instant.now()).toMillis}ms")
    //val (result1, result2) = ("", "")
    (s"${result1}", s"${result2}")


object IdiomaticSolutionBrutForce:
  def solveFromFilteringMaps(value: Long, filters: Seq[SolvingMaps]): Long =
    filters.foldLeft(value)((acc, newVal) => newVal.solve(acc))
  def solve(seeds: Seq[Long] , filters: Seq[SolvingMaps]): (String, String) =
    val result1 = seeds.map:
      case value => solveFromFilteringMaps(value, filters)
    .min

    val Seq(seedsEven, seedsOdd) = List(0, 1).map(modulo => seeds.zipWithIndex.filter((value, index) => index % 2 == modulo).map(_._1))

    val result2 = (seedsEven.zip(seedsOdd)).par.map { (start, length) =>
      val partialResult = (start until start+length).sliding(30).map { outerList =>
        outerList.par.map {
          solveFromFilteringMaps(_, filters)
        }.min
      }.minOption.getOrElse(Long.MaxValue)
      println(s"partialResult : $partialResult from $start to ${start+length}")
      partialResult
    }.min

    (s"${result1}", s"${result2}")

object IdiomaticSolution:
  def mergeMaps(mapLineToMergeWith: MapLine, mapLineToMerge: MapLine): Seq[MapLine] =
    def intersect: Boolean =
      def test(start1: Long, end1: Long, start2: Long, end2: Long): Boolean =
        start2 >= start1 && end2 <= end1
      val (start1, end1) = (mapLineToMergeWith.sourceStartProjected, mapLineToMergeWith.sourceEndProjected)
      val (start2, end2) = (mapLineToMerge.sourceStart, mapLineToMerge.sourceEnd)

      test(start1, end1, start2, end2) || test(start2, end2, start1, end1)
    if (intersect)
      (mapLineToMergeWith, mapLineToMerge) match
        case (map1, map2) if map1.startsBefore(map2) => map1.cutAtStart(map2) :: mergeMaps(map1.fromStart(map2), map2)
        case (map1, map2) if map1.endsAfter(map2) => mergeMaps(map1.tilEnd(map2), map2) :: map1.fromEnd(map2)
        case (map1, map2) if map1.containsProjected(map2) => mergeMaps(mapLineToMergeWith.cutAtStart(map2), map2) :: map1.mergeWithSameStartAndEnd.copy(sourceStart = map1.sourceStart + map2.sourceStart-map1.sourceStartProjected, destination = map1.destination + map2.destination, sourceLength = map2.sourceLength) :: map1.copy(sourceStart = map1.sourceStart + map2.sourceLength + map1.sourceStartProjected - map2.sourceStart, sourceLength = map1.sourceLength - map1.sourceStart + map2.sourceLength + map1.sourceStartProjected - map2.sourceStart)
        case (map1, map2) if map2.containsProjected(map1) => map2.copy(sourceLength = map2.sourceStart - map1.sourceStart) : map1.copy(destination = map1.destination + map2.destination) :: map2.copy(sourceStart = map1.sourceEnd + 1, sourceLength = map2.sourceLength - map1.sourceEnd + 1)
        case (map1, map2) if map1.startsBefore(map2) => map1.copy(sourceLength = map2.sourceStart-map1.sourceStart) :: map2.copy()
        case (map1, map2) => map2 :: map1.copy(sourceLength = map2.sourceEnd - map1.sourceStart, destination = map2.destination) :: map1.copy(sourceStart = map2.sourceEnd, sourceLength = map2.sourceEnd - map1.sourceLength)
    else
        Nil :: mapLineToMergeWith

  def solveFromFilteringMaps(value: Long, filters: Seq[SolvingMaps]): Long =
    filters.foldLeft(value)((acc, newVal) => newVal.solve(acc))
  def solve(seeds: Seq[Long] , filters: Seq[SolvingMaps]): (String, String) =
    val result1 = seeds.map:
      case value => solveFromFilteringMaps(value, filters)
    .min

    val Seq(seedsEven, seedsOdd) = List(0, 1).map(modulo => seeds.zipWithIndex.filter((value, index) => index % 2 == modulo).map(_._1))

    val result2 = (seedsEven.zip(seedsOdd)).par.map { (start, length) =>
      val partialResult = (start until start+length).sliding(30).map { outerList =>
        outerList.par.map {
          solveFromFilteringMaps(_, filters)
        }.min
      }.minOption.getOrElse(Long.MaxValue)
      println(s"partialResult : $partialResult from $start to ${start+length}")
      partialResult
    }.min

    (s"${result1}", s"${result2}")

object DumbButFastSolution:
  def initializeArrayFromFilteringMaps(filters: Seq[SolvingMaps]): Array[Array[Array[Long]]] =
    val initializedArray = Array.fill[Long](filters.length, filters.map(_.length).max + 1, 3)(-1)
    filters.zipWithIndex.foreach: (filter, indexOfFilter) =>
      filter.linesMapped.zipWithIndex.foreach: (mapper, indexOfMapper) =>
        initializedArray(indexOfFilter)(indexOfMapper)(0) = mapper.destination
        initializedArray(indexOfFilter)(indexOfMapper)(1) = mapper.sourceStart
        initializedArray(indexOfFilter)(indexOfMapper)(2) = mapper.sourceLength
    initializedArray


  def solve(seeds: Seq[Long], filters: Seq[SolvingMaps]): (String, String) =
    val numberOfFilters = filters.length
    val workingArray =  initializeArrayFromFilteringMaps(filters)
    def resolve(value: Long): Long =
      var valueFound = value
      var step = 0
      while(step < numberOfFilters) {
        var subFilterIndex = 0
        var stop = false
        while(workingArray(step)(subFilterIndex)(0) != -1 && stop == false) {
          val sourceStart = workingArray(step)(subFilterIndex)(1)
          val sourceLength = workingArray(step)(subFilterIndex)(2)
          if (valueFound >= sourceStart && valueFound < sourceStart + sourceLength)
            valueFound = workingArray(step)(subFilterIndex)(0) + valueFound - sourceStart
            stop = true
          subFilterIndex = subFilterIndex+1
        }
        step = step+1
      }
      valueFound

    val result1 = seeds.map:
      case value => resolve(value)
    .min

    //equivalent to
    // val (seedsEven, seedsOdd) = seeds.partition(_%2 == 0)
    // seedsEven.zip(seedsOdd).par.map { (start, length) =>
    val result2 = seeds.grouped(2).toSeq.par.map { case Seq(start, length) =>
      var i: Long = start
      var minimum: Long = Long.MaxValue
      while (i < start + length) {
        minimum = min(minimum, resolve(i.toLong))
        i = i + 1
      }
      println(s"partialResult : $minimum from $start to ${start + length} in ${Duration.between(startTime, Instant.now()).toMillis}ms")
      minimum
    }.min


    (s"${result1}", s"${result2}")

case class Range(start: Long, end: Option[Long]):
  def matches(other: Range): (Option[Range], Option[Range], Option[Range]) =
    val part1 = other match
      case value if this.startsBeforeStrictly(value) => Some(this.copy(end= Some(value.start-1)))
      case _ => None
    val part2 = other match
      case value if this.overlaps(value) => Some(this.copy(value.start, end = this.firstEnd(value)))
      case _ => None
    val part3 = other match
      case value if this.endsAfterStrictly(value) => Some(this.copy(start = value.end.get + 1))
      case _ => None
  (part1, part2, part3)

  def length: Option[Long] = end.map(_-start)
  def left(other: Range): Range =
    Range(start, other.start - 1)
  def right(other: Range): Range =
     Range(other.end + 1, end)
  def startsTogether(other: Range): Boolean =
    start == other.start
  def endsAfterStrictly(other: Range): Boolean =
    end match
      case None => other.end.isDefined
      case Some(value) => other.end.map(value > _).getOrElse(false)
  def startsBeforeStrictly(other: Range): Boolean =
    start < other.start
  def firstEnd(other: Range): Option[Long] =
    (end, other.end) match
      case (None, None) => None
      case (Some(value), None) => Some(value)
      case (None, Some(value)) => Some(value)
      case (Some(value), Some(valueOther)) => Some(min(value, valueOther))
  def overlaps(other: Range): Boolean =
    this.startsBefore(other) && this.endsAfterStartOf(other) || other.startsBefore(this) && other.endsAfterStartOf(this)
  def endsAfter(other: Range): Boolean =
    end match
      case None => true
      case Some(value) => other.end.map(value>=_).getOrElse(false)
  def startsBefore(other: Range): Boolean =
    start <= other.start
  def endsAfterStartOf(other: Range): Boolean =
    end match
      case None => true
      case Some(value) => value >= other.start
  def endsTogether(other: Range): Boolean =
    end match
      case Some(value) => other.end.contains(value)
      case None => other.end.isEmpty
  def contains(other: Range): Boolean =
    start <= other.start && end.map(_ >= other.end).getOrElse(true)

object Range:
  def fromMapLine(mapLine: MapLine): Range =
    Range(mapLine.sourceStart, mapLine.sourceEnd)

case class SeedToLocation(range: Range, drift: Long):
  def rangeDrifted: Range =
    range.copy(start = range.start+drift, end.map(_+drift))
  def rangeUndrifted(other: Range): Range =
    range.copy(start = other.start - this.drift, end.map(_ - this.drift))
  def compose(mapLine: MapLine): Seq[SeedToLocation] =
    this.rangeDrifted.matches(fromMapLine(mapLine)) match
      case (None, None, None) => Nil :: this
      case (None, Some(value), None) => Nil :: this.copy(this.drift+mapLine.drift)
      case (Some(valueFirst), Some(valueInner), None) => this.copy(range = rangeUndrifted(valueFirst)) :: SeedToLocation(rangeUndrifted(valueInner), this.drift+mapLine.drift)
      case (None, Some(valueInner), Some(valueLast)) => SeedToLocation(rangeUndrifted(valueInner), this.drift+mapLine.drift) :: this.copy(range = rangeUndrifted(valueLast))
      case (Some(valueFirst), Some(valueInner), Some(valueLast)) => this.copy(range = rangeUndrifted(valueFirst)) :: SeedToLocation(rangeUndrifted(valueInner), this.drift+mapLine.drift) :: this.copy(range = rangeUndrifted(valueLast))
      case _ => Nil

  def containsFully(mapLine: MapLine): Boolean =
    rangeDrifted.contains(Range.fromMapLine(mapToAdd))
  def startsBefore(mapLine: MapLine): Boolean =
    rangeDrifted.startsBefore(Range.fromMapLine(mapToAdd))
  def endsAfter(mapLine: MapLine): Boolean =
    rangeDrifted.endsAfter(Range.fromMapLine(mapToAdd))

  def splitInThreeFromMapLine(mapLine: MapLine): Seq[SeedToLocation] =
    def firstPart: Seq[SeedToLocation] =
      (range left Range.fromMapLine(mapLine)).length match
        case _ < 1 => Nil
        case value => SeedToLocation(value, drift)
    def lastPart: Seq[SeedToLocation] =
      (range - Range.fromMapLine(mapLine)).length match
        case _ < 1  => Nil
        case value => SeedToLocation(value, drift)
    firstPart :: compose(mapLine) :: lastPart


  def solve(value: Long): Option[Long] =
    if (value >= range.start && range.end.map(_<= value).getOrElse(true))
      Some(value + drift)
    else
      None

object SeedToLocation:
  def default = SeedToLocation(Range(1, None), 0)

case class SeedToLocationHolder(stl: Seq[SeedToLocation]):
  def add(newSolvingMap: SolvingMaps): SeedToLocationHolder =
    newSolvingMap.linesMapped.flatMap()

object SeedToLocationHolder:
  def default = SeedToLocationHolder(Seq(SeedToLocation.default))

case class MultilevelSolvingMap(data: Seq[SolvingMaps]):
  def build: SeedToLocationHolder =
    data.fold(SeedToLocationHolder.default) { (acc, newSolvingMap) =>
      acc.add(newSolvingMap)
    }
  def solve(value: Long): Long =
    1l

case class SolvingMaps(name: String, lines: Seq[String]):
  def parseLines: Seq[MapLine] =
    lines.map:
      case s"$destination,$sourceStart,$sourceLength" => MapLine(destination.toLong, sourceStart.toLong, sourceLength.toLong)

  override def toString(): String = s"$name, nb of lines ${linesMapped.length}"
  val linesMapped: Seq[MapLine] = parseLines
  def length = linesMapped.length
  def solve(value: Long): Long =
    val result = linesMapped.map(_.solve(value)).find(_.isDefined).map(_.get).getOrElse(value)
    //println(s"")
    result

case class MapLine(destination: Long, sourceStart: Long, sourceLength: Long):
  def drift: Long = destination - sourceStart
  def sourceEnd: Long = sourceStart + sourceLength - 1
  def sourceStartProjected: Long = sourceStart + destination
  def sourceEndProjected: Long = sourceEnd +destination
  def mergeWithSameStartAndEnd(other: MapLine): MapLine =
    this.copy(destination = this.destination + other.drift)
  def containsProjected(other: MapLine): Boolean =
    other.sourceStart >= this.sourceStartProjected && other.sourceEnd <= this.sourceEndProjected
  def startsBeforeProjected(other: MapLine): Boolean =
    other.sourceStart >= this.sourceStartProjected
  def endsBeforeProjected(other: MapLine): Boolean =
    other.sourceEnd <= this.sourceEndProjected
  def cutAtStart(other: MapLine) =
    this.copy(sourceLength = other.sourceStart-sourceStartProjected)
  def fromStart(other: MapLine) =
    driftOf(other.sourceStart - sourceStartProjected)
  def driftOf(value: Long) =
    MapLine(destination+value, sourceStart+value, sourceLength-value)
  def cutFromEnd(other: MapLine) =
      val newStart = other.sourceEnd + 1
      val distanceBetweenStarts = newStart - this.sourceStart
      val newLength = this.sourceLength - distanceBetweenStarts
      val newDestination = destination-sourceStart+distanceBetweenStarts
      MapLine(sourceStart = newStart, sourceLength = newLength, destination = newDestination)
  def solve(value: Long): Option[Long] =
    val result = value-sourceStart  match
      case toTest if toTest >= 0 && toTest < sourceLength => Some(destination + toTest)
      case _ => None
    //print(s"${result.getOrElse(value)}-")
    result

