import java.awt.Component

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    println(WireBox.from(inputLines))


    val result1 = s""
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

case class WireBox(wires: Seq[Wire])

object WireBox:
  def from(input: Seq[String]): WireBox =
    def from(singleInput: String): List[Wire] =
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

case class Wire(ends: Set[Component]):
  require(ends.size == 2)


