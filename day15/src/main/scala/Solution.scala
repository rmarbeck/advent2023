object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val inputRawLenses = inputLines.head.split(",")

    val resultPart1 = inputRawLenses.map(hash).sum

    val boxHolder = inputRawLenses.map(Lens.from(_)).foldLeft(BoxHolder(256)):
      (acc, newLens) => acc.manage(newLens)

    val resultPart2 = boxHolder.focusingPower

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

end Solution

class BoxHolder(size: Int):
  private val boxes : Array[Box] = Array.tabulate(size)(Box(_))

  def focusingPower: Int = boxes.map(_.focusingPower).sum

  def manage(newLens: Lens): BoxHolder =
    boxes(newLens.hashed) = boxes(newLens.hashed).manage(newLens)
    this

case class Lens(label: String, value: Option[Int])

object Lens:
  def from(raw: String): Lens =
    raw match
      case s"$label-" => Lens(label, None)
      case s"$label=$value" => Lens(label, Some(value.toInt))
      case _ => throw Exception("Not managed")

extension (l: Lens)
  def hashed: Int = hash(l.label)

case class Box(id: Int, lenses: Seq[Lens] = Seq()):
  lazy val focusingPower = (id + 1) * lenses.zipWithIndex.map((lens, index) => lens.value.getOrElse(0) * (index + 1)).sum

  def manage(newLens: Lens): Box =
    newLens match
      case Lens(label, None) => removeLens(label)
      case Lens(label, Some(value)) => updateLens(label, value)

  private def removeLens(label: String): Box = this.copy(lenses = lenses.filterNot(_.label == label))
  private def updateLens(label: String, value: Int): Box =
    lenses.map(_.label).contains(label) match
      case true =>
        this.copy(lenses = lenses.map:
          case Lens(lbl, _) if lbl == label => Lens(label, Some(value))
          case lens => lens
        )
      case false => this.copy(lenses = lenses :+ Lens(label, Some(value)))

def hash(input: String): Int =
  input.foldLeft(0):
    (acc, newChar) => (acc + newChar.toInt) * 17 % 256