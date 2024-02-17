
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

    val layers = mappers.map(SingleLayerRangeSet.from)

    val finalLayer = MultiLayerSolver(layers.toList)

    val resultPart1 = seeds.map(finalLayer.solve).min

    val resultPart2 = seeds.grouped(2).map:
      case Seq(first: Long, second: Long) => finalLayer.solveRange(first, first + second)
    .min

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

case class Mapping(destinationStart: Long, sourceStart: Long, length: Long)

case class Mapper(name: String, mappings: List[Mapping])
object Mapper:
  def fromString(formatedInput: String): Mapper =
    formatedInput match
      case s"$name map:/$mappings" =>
        val foundMappings = mappings.split("/").map:
          case s"$destination $source $length" => Mapping(destination.toLong, source.toLong, length.toLong)
          case _ => throw Exception("Not supported format")
        Mapper(name, foundMappings.toList)
      case _ => throw Exception("Not supported format")
