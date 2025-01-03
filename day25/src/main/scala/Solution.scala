
import scala.concurrent.Await
import org.apache.pekko
import pekko.util.Timeout
import concurrent.duration.DurationInt

enum Method:
  case withPekko, withStoer

val chooseSolution: Method = Method.withPekko

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result1 = chooseSolution match
      case Method.withPekko => solveWithPekko(inputLines)
      case _ => solve(inputLines)

    val result2 = s"Happy Christmas"

    (s"$result1", s"$result2")

def solve(inputLines: Seq[String]): String =
  val edges =
    inputLines.collect:
      case s"$source: $destinations" =>
        destinations.split(" ").map(dest => Edge(Set(Vertice(dest), Vertice(source)), 1))
    .flatten

  val graph = Graph(edges.toSet)

  val resultPart1 = minCut(graph)

  s"${(graph.vertices.size-resultPart1._2) * resultPart1._2}"

def solveWithPekko(inputLines: Seq[String]): String =
  val wirebox = WireBox.from(inputLines)

  import concurrent.ExecutionContext.Implicits.global
  val futureResultPart1 = PekkoLauncher.runThroughPekko(wirebox)(using timeout = 120.seconds).map:
    nbOnOneSide =>
      val nbOnOtherSide = wirebox.nbOfEdges - nbOnOneSide
      nbOnOtherSide * nbOnOneSide

  val resultPart1 =
    try {
      Await.result(futureResultPart1, 110.seconds).toString
    } catch
      case exception: Exception => "Not found within time limit"

  resultPart1


case class WireBox(wires: Seq[Wire[_]]):
  def nbOfEdges = wires.flatMap(_.ends).distinct.size

object WireBox:
  def from(input: Seq[String]): WireBox =
    def from(singleInput: String): List[Wire[_]] =
      singleInput match
        case s"$first: $others" =>
          val firstComponent = Component(first)
          for
            other <- others.split(" ").toList
            otherComponent = Component(other)
          yield
            Wire(Set(firstComponent, otherComponent))

    WireBox(input.map(from).flatten)

case class Component(name: String)

case class Wire[A <: Component](ends: Set[A]):
  require(ends.size == 2)
  private lazy val endsIndexed = ends.toIndexedSeq

  def otherThan(component: A): Option[A] =
    endsIndexed.indexOf(component) match
      case -1 => None
      case 0 => Some(endsIndexed(1))
      case 1 => Some(endsIndexed(0))

  override def toString: String = ends.mkString(",")


