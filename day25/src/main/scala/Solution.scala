import java.awt.Component

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val wirebox = WireBox.from(inputLines)

    val maxRandomTries = 3000
    val nbOfCuts = 3

    val searchResult = MinCutRandom(SimpleGraphForRandom(wirebox), nbOfCuts, maxRandomTries)

    val resultPar1 = searchResult match
      case Some(nbOnOneSide, _) =>
        val nbOnOtherSide = wirebox.nbOfEdges - nbOnOneSide
        nbOnOtherSide * nbOnOneSide
      case _ => throw Exception("Not found")


    val result1 = s"$resultPar1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

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


