
val MULTIPLIER = 17
val MAX_SIZE = 256

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val inputRawLenses = inputLines.head.split(",")

    val result1 = inputRawLenses.map(hash).sum

    val boxHolder = inputRawLenses.map(Lens.from).foldLeft(BoxHolder.empty(MAX_SIZE)):
      (acc, newLens) => acc.manage(newLens)

    val result2 = boxHolder.focusingPower

    (s"$result1", s"$result2")

class BoxHolder private(boxes: Vector[Box]):
  def focusingPower: Int = boxes.map(_.focusingPower).sum

  def manage(newLens: Lens): BoxHolder = BoxHolder(boxes.updated(newLens.hashed, boxes(newLens.hashed).manage(newLens)))

object BoxHolder:
  def empty(size: Int): BoxHolder = new BoxHolder(Vector.tabulate(size)(Box(_)))

case class Lens(label: String, value: Option[Int]):
  lazy val hashed: Int = hash(label)

object Lens:
  def from(raw: String): Lens =
    raw match
      case s"$label-" => Lens(label, None)
      case s"$label=$value" => Lens(label, Some(value.toInt))
      case _ => throw Exception("Not managed")

case class Box(id: Int, lenses: Seq[Lens] = Seq()):
  lazy val focusingPower: Int = (id + 1) * lenses.zipWithIndex.map((lens, index) => lens.value.getOrElse(0) * (index + 1)).sum

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
    (acc, newChar) => (acc + newChar.toInt) * MULTIPLIER % MAX_SIZE