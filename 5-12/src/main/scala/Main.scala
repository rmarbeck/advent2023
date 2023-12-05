import scala.io.Source
import scala.math._

import java.time.Duration
import java.time.Instant

import scala.collection.parallel._
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


object IdiomaticSolution:
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
  def solve(value: Long): Option[Long] =
    val result = value-sourceStart  match
      case toTest if toTest >= 0 && toTest < sourceLength => Some(destination + toTest)
      case _ => None
    //print(s"${result.getOrElse(value)}-")
    result
