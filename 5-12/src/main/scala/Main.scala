import IdiomaticSolutionBrutForce.solveFromFilteringMaps

import scala.io.Source
import scala.math.*
import java.time.Duration
import java.time.Instant
import javax.print.attribute.standard.MediaSize.Other
import scala.collection.immutable.Seq
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
    val (result1, result2) = IdiomaticSolution.solve(seeds, filteringMaps)
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
      //println(s"partialResult : $partialResult from $start to ${start+length}")
      partialResult
    }.min

    (s"${result1}", s"${result2}")

object IdiomaticSolution:
  def solve(seeds: Seq[Long] , filters: Seq[SolvingMaps]): (String, String) =
    val solver = MultilevelSolvingMap(filters)
    val result1 = seeds.map:
      case value => {
        val result = solver.solve(value)
        //println(s"$value => $result")
        result
      }
    .min

    val Seq(seedsEven, seedsOdd) = List(0, 1).map(modulo => seeds.zipWithIndex.filter((value, index) => index % 2 == modulo).map(_._1))

    val result2 = (seedsEven.zip(seedsOdd)).map { (start, length) =>
        val partialResult = solver.solve(Range(start, Some(start+length-1)))
        //println(s" $start \t${start+length-1} => \t\t$partialResult")
        partialResult
      }.minOption.getOrElse(Long.MaxValue)


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

case class Range(start: Long, ending: Option[Long]):
  def matches(other: Range): (Option[Range], Option[Range], Option[Range]) =
    if (!this.overlaps(other)) {
      (None, None, None)
    } else {
      val part1 = other match
        case value if this.startsBeforeStrictly(value) => Some(this.copy(ending= Some(value.start-1)))
        case _ => None
      val part2 = other match
        case value if this.overlaps(value) => Some(this.copy(max(this.start,value.start), ending = this.firstEnd(value)))
        case _ => None
      val part3 = other match
        case value if this.endsAfterStrictly(value) => Some(this.copy(start = value.ending.get + 1))
        case _ => None
      (part1, part2, part3)
    }

  def length: Option[Long] = ending.map(_-start)

  def startsTogether(other: Range): Boolean =
    start == other.start
  def endsAfterStrictly(other: Range): Boolean =
    ending match
      case None => other.ending.isDefined
      case Some(value) => other.ending.map(value > _).getOrElse(false)
  def startsBeforeStrictly(other: Range): Boolean =
    start < other.start
  def firstEnd(other: Range): Option[Long] =
    (ending, other.ending) match
      case (None, None) => None
      case (Some(value), None) => Some(value)
      case (None, Some(value)) => Some(value)
      case (Some(value), Some(valueOther)) => Some(min(value, valueOther))
  def overlaps(other: Range): Boolean =
    this.startsBefore(other) && this.endsAfterStartOf(other) || other.startsBefore(this) && other.endsAfterStartOf(this)
  def endsAfter(other: Range): Boolean =
    ending match
      case None => true
      case Some(value) => other.ending.map(value>=_).getOrElse(false)
  def startsBefore(other: Range): Boolean =
    start <= other.start
  def endsAfterStartOf(other: Range): Boolean =
    ending match
      case None => true
      case Some(value) => value >= other.start
  def endsTogether(other: Range): Boolean =
    ending match
      case Some(value) => other.ending.contains(value)
      case None => other.ending.isEmpty


object Range:
  def fromMapLine(mapLine: MapLine): Range =
    Range(mapLine.sourceStart, Some(mapLine.sourceEnd))

case class SeedToLocation(range: Range, drift: Long, level: Int):
  def rangeDrifted: Range =
    range.copy(start = range.start+drift, range.ending.map(_+drift))
  def rangeUndrifted(other: Range): Range =
    range.copy(start = other.start - this.drift, other.ending.map(_ - this.drift))
  def rangeUndrifted(other: Option[Range]): Option[Range] =
    other match
      case None => None
      case Some(value) => Some(rangeUndrifted(value))
  def compose(mapLine: MapLine, composingLevel: Int): Seq[SeedToLocation] =
    def createWithNewDrift(range: Option[Range], newDrift: Long): Seq[SeedToLocation] =
      range match
        case None => Nil
        case Some(validRange) if newDrift != 0 => Seq(this.copy(range = validRange, drift = this.drift + newDrift, level = composingLevel+1))
        case Some(validRange) => Seq(this.copy(range = validRange, drift = this.drift, level = composingLevel))
    def createWithoutNewDrift(range: Option[Range]): Seq[SeedToLocation] = createWithNewDrift(range, 0l)

    if (this.level > composingLevel) {
      this +: Nil
    } else {
      this.rangeDrifted.matches(Range.fromMapLine(mapLine)).toList.map(rangeUndrifted(_)).match {
        case List(a_undrifted, b_undrifted, c_undrifted) => (a_undrifted, b_undrifted, c_undrifted)
        case _ => (None, None, None)
      }.match
        case (None, None, None) => this +: Nil
        case (valueStart, Some(value), valueEnd) => {
          createWithoutNewDrift(valueStart) ++ createWithNewDrift(Some(value), mapLine.drift) ++ createWithoutNewDrift(valueEnd) ++ Nil
        }
        case value => println(s"IN this case ${value}"); Nil
    }

  def solve(value: Long): Option[Long] =
    if (value >= range.start && range.ending.map(value <= _).getOrElse(true))
      Some(value + drift)
    else
      None

  def solve(value: Range): Option[Long] =
    if (value.overlaps(range))
      //println(s"found ${value} => ${range.start + drift}")
      Some(range.start + drift)
    else
      None

  override def toString: String = s"(${range.start}, ${range.ending.getOrElse("oo")}) -> (${range.start+drift}, ${range.ending.map(_+drift).getOrElse("oo")}) [$level]"

object SeedToLocation:
  def default = SeedToLocation(Range(0, None), 0, 0)

case class SeedToLocationHolder(stl: Seq[SeedToLocation], level: Int):
  def add(newSolvingMap: SolvingMaps): SeedToLocationHolder =
    //SeedToLocationHolder(stl.flatMap(eachPart => newSolvingMap.linesMapped.flatMap(eachPart.compose(_, level))), level + 1)
    SeedToLocationHolder(newSolvingMap.linesMapped.foldLeft(stl) {
      (acc, newValue) =>
        //println(s"===> adding $newValue");
        val result = acc.flatMap(_.compose(newValue, level))
        //println(s"===> added $newValue");
        //println(s"$result");
        result
    }, level + 1)

object SeedToLocationHolder:
  def default = SeedToLocationHolder(Seq(SeedToLocation.default), 0)

case class MultilevelSolvingMap(data: Seq[SolvingMaps]):
  val gardener = build
  def build: SeedToLocationHolder =
    data.foldLeft(SeedToLocationHolder.default) { (acc, newSolvingMap) =>
      //println(s"$acc adding $newSolvingMap")
      val result = acc.add(newSolvingMap)
      //result.stl.foreach(println)
      //println("...............")
      result

    }
  def solve(value: Long): Long =
    gardener.stl.map(_.solve(value)).find(_.isDefined).headOption match
      case None => 0l
      case Some(value) => value.getOrElse(0l)

  def solve(value: Range): Long =
    gardener.stl.map(_.solve(value)).filter(_.isDefined).map(_.get).min

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

