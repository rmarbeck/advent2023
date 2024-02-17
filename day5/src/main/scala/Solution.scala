import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (rawSeeds, rawMappers) = inputLines.splitAt(2)

    val seeds: Seq[Long] =
      rawSeeds.head match
        case s"seeds: $values" => values.split(" ").map(_.toLong).toIndexedSeq
        case _ => throw Exception("Seeds not recognized")

    val mappers = rawMappers.map:
      case currentLine if currentLine.isEmpty => "#"
      case currentLine => currentLine + "/"
    .mkString
    .split("#")
    .map(Mapper.fromString)

    val mappingService = MappingService(mappers)

    val resultPart1 = seeds.map(mappingService.map(_)).min

    /*val mappingFunctions = mappers.map(MappingFunction.fromMapper)

    val aggregate = mappingFunctions.tail.foldLeft(mappingFunctions.head):
      case (acc, newFunction) => acc.digest(newFunction)

    println(s"intermediate is ${aggregate.smallestFrom(79, 93)}")
    println(s"intermediate is ${aggregate.smallestFrom(55, 68)}")*/

    val layers = mappers.map(SingleLayerRangeSet.from)

    val finalLayer = MultiLayerSolver.merge(layers.toList)


    //println(s" OK ${finalLayer}")
    //println(s" NOT OK ${MultiLayerSolver.merge(layers.toList.dropRight(1))}")

    /*println(s"value of 82 is ${finalLayer.solve(82)}")
    println(s"value of 79 is ${finalLayer.solve(79)}")
    println(s"value of 14 is ${finalLayer.solve(14)}")
    println(s"value of 55 is ${finalLayer.solve(55)}")
    println(s"value of 13 is ${finalLayer.solve(13)}")

    println(s"intermediate is ${finalLayer.solveRange(79, 93)}")
    println(s"intermediate is ${finalLayer.solveRange(55, 68)}")*/

    val result = seeds.toList.sliding(2, 2).map:
      case List(first: Long, second: Long) => finalLayer.solveRange(first, first + second)
      case value => throw Exception(s"Should not happen : $value")
    .min

    val resultPart2 = result

    /*val seedsPart2 = seeds.toList.sliding(2, 2).map:
      case first :: second :: Nil => first until first + second

    val resultPart2 = seedsPart2.map:
      case currentSeedRange =>
        currentSeedRange.foldLeft(Long.MaxValue):
          case (acc, seed) => math.min(acc, mappingService.map(seed))
    .min*/

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

class MappingService(val mappers: Array[Mapper]):
  val cachedMap: mutable.Map[Long, Long] = mutable.HashMap[Long, Long]()

  def map(source: Long): Long =
    //cachedMap.getOrElseUpdate(source, doLocate(source))
    doLocate(source)

  private def doLocate(seedValue: Long): Long =
    mappers.foldLeft(seedValue):
      case (acc, mapper) => mapper.map(acc)

case class Mapping(destinationStart: Long, sourceStart: Long, length: Long):
  def matches(source: Long): Boolean =
    source - sourceStart match
      case value if value < 0 => false
      case value if value > length => false
      case _ => true
  def follow(source: Long): Long =
    matches(source) match
      case false => source
      case true => destinationStart + source - sourceStart

  def mergeWithPreviousLevel(previousMapping: Mapping): (Option[Mapping], Option[Mapping], Option[Mapping]) = ???

case class Mapper(name: String, mappings: List[Mapping]):
  def map(source: Long): Long =
    mappings.find(_.matches(source)) match
      case Some(mapping) => mapping.follow(source)
      case None => source

  def mergeWith(other: Mapper): Mapper =
    def from(mapping: Mapping): List[Mapping] =
      val (initialDestination, initialSource, initialLength) = (mapping.destinationStart, mapping.sourceStart, mapping.length)

      Nil

    val finalMappings = this.mappings.flatMap(from(_))
    Mapper(this.name+"-"+other.name, finalMappings)

object Mapper:
  def fromString(formatedInput: String): Mapper =
    formatedInput match
      case s"$name map:/$mappings" =>
        val foundMappings = mappings.split("/").map:
          case s"$destination $source $length" => Mapping(destination.toLong, source.toLong, length.toLong)
          case _ => throw Exception("Not supported format")
        Mapper(name, foundMappings.toList)
      case _ => throw Exception("Not supported format")
