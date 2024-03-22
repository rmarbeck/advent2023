import scala.collection.immutable.{BitSet, HashMap}

class MergeableInt(val value: Int, val mergingCounter: Long = 0) extends Mergeable[MergeableInt]:
  override def mergeWith(other: MergeableInt): MergeableInt =
    val newValue = this.value
    MergeableInt(newValue, mergingCounter + other.mergingCounter + 1)
  override def nbMerge: Long = mergingCounter

  override def toString: String = s"$value ($mergingCounter)"

object MergeableInt:
  def apply(value: Int, mergingCounter: Long): MergeableInt = new MergeableInt(value, mergingCounter)
  def apply(value: Int): MergeableInt = new MergeableInt(value)



class MutableBitSetWireBlackBox(summits: scala.collection.mutable.Map[Int, MergeableInt], wires: scala.collection.mutable.Map[BitSet, Int]) extends WireBlackBox[MergeableInt]:
  def flattenEdges: IndexedSeq[BitSet] =
    wires.keys.toIndexedSeq.flatMap:
      key => List.fill(wires(key))(key)

  override def getConnectedElementsOnEdge(index: Int): (MergeableInt, MergeableInt) =
    flattenEdges(index) match
      case value if value.size == 2 => (summits(value.head), summits(value.last))
      case _ => throw Exception("BitSet is not a two elements BitSet")

  override def getSummits: Seq[MergeableInt] = summits.values.toSeq

  override def merge(first: MergeableInt, second: MergeableInt): WireBlackBox[MergeableInt] =
    val mergedComponent = first.mergeWith(second)
    val (firstAsInt, secondAsInt, mergedAsInt) = (first.value, second.value, mergedComponent.value)
    summits.remove(secondAsInt)
    summits.update(mergedAsInt, mergedComponent)

    wires.view.filter((key, value) => key.contains(firstAsInt) || key.contains(secondAsInt)).foreach:
      case (key, value) if key == BitSet(firstAsInt, secondAsInt) => wires.remove(key)
      case (key, value) =>
        val other = key.excl(secondAsInt).excl(firstAsInt).head
        wires.update(BitSet(mergedAsInt, other), wires.getOrElse(BitSet(firstAsInt, other), 0) + wires.getOrElse(BitSet(secondAsInt, other), 0))
        wires.remove(BitSet(secondAsInt, other))

    this

  override def nbOfEdges: Long = flattenEdges.size.toLong

object MutableBitSetWireBlackBox:
  def from(wireBox: WireBox): MutableBitSetWireBlackBox =
    val immutableOne = BitSetWireBlackBox.from(wireBox)
    val mutableSummits = scala.collection.mutable.Map[Int, MergeableInt](immutableOne.summits.toSeq:_*)
    val mutableWires = scala.collection.mutable.Map[BitSet, Int](immutableOne.wires.toSeq:_*)
    MutableBitSetWireBlackBox(mutableSummits, mutableWires)


case class BitSetWireBlackBox(summits: Map[Int, MergeableInt], wires: Map[BitSet, Int]) extends WireBlackBox[MergeableInt]:
  lazy val flattenEdges =
    wires.keys.toIndexedSeq.flatMap:
      key => List.fill(wires(key))(key)

  override def getConnectedElementsOnEdge(index: Int): (MergeableInt, MergeableInt) =
    flattenEdges(index) match
      case value if value.size == 2 => (summits(value.head), summits(value.last))
      case _ => throw Exception("BitSet is not a two elements BitSet")

  override def getSummits: Seq[MergeableInt] = summits.values.toSeq

  override def merge(first: MergeableInt, second: MergeableInt): WireBlackBox[MergeableInt] =
    val mergedComponent = first.mergeWith(second)
    val (firstAsInt, secondAsInt, mergedAsInt) = (first.value, second.value, mergedComponent.value)
    val newSummits = summits.filterNot((key, value) => value == second || value == first) + (mergedAsInt -> mergedComponent)
    val (toUpdate, unTouched) =
      wires.partition:
        wire => wire._1.contains(firstAsInt) || wire._1.contains(secondAsInt)


    val updated = toUpdate.foldLeft(Map[BitSet, Int]()):
      case (acc, (key, value)) if key == BitSet(firstAsInt, secondAsInt) => acc
      case (acc, (key, value)) =>
        val other = key.excl(secondAsInt).excl(firstAsInt).head
        acc.get(BitSet(mergedAsInt, other)) match
          case Some(previous) => acc.filterNot((key, _) => key == BitSet(mergedAsInt, other)) + (BitSet(mergedAsInt, other) -> (previous + value))
          case None => acc + (BitSet(mergedAsInt, other) -> value)


    BitSetWireBlackBox(newSummits, unTouched ++ updated)

  override def nbOfEdges: Long = flattenEdges.size.toLong

object BitSetWireBlackBox:
  def from(wireBox: WireBox): BitSetWireBlackBox =
    val summitsNames =
      wireBox.wires.toIndexedSeq.flatMap:
        wire => wire.ends.map:
          case component: Component => component.name
      .distinct

    val summits = HashMap[Int, MergeableInt] (
      (for
        (summitName, index) <- summitsNames.zipWithIndex
      yield
        index -> MergeableInt(index)
        ): _*
    )

    val wires = HashMap[BitSet, Int] (
      (for
        wire <- wireBox.wires
      yield
        val (index1, index2) = (summitsNames.indexOf(wire.ends.head.name), summitsNames.indexOf(wire.ends.last.name))
        (BitSet(index1, index2) -> 1)
        ): _*
    )


    BitSetWireBlackBox(summits, wires)


class BitSetGraphForRandom(wireBlackBox: WireBlackBox[MergeableInt]) extends GraphForRandom[MergeableInt]:
  override lazy val getElements: Seq[MergeableInt] = wireBlackBox.getSummits

  override lazy val nbOfEdges: Long = wireBlackBox.nbOfEdges

  override def cutBetweenTwoEdges: Either[String, Long] =
    getElements.size match
      case 2 => Right(nbOfEdges)
      case value => Left(s"Not supported, should have 2 elements and has $value")

  override def getConnectedElementsOnEdge(index: Int): (MergeableInt, MergeableInt) = wireBlackBox.getConnectedElementsOnEdge(index)

  override def merge(one: MergeableInt, other: MergeableInt) = BitSetGraphForRandom(wireBlackBox.merge(one, other))

object BitSetGraphForRandom:
  def apply(wireBlackBox: WireBlackBox[MergeableInt]): BitSetGraphForRandom = new BitSetGraphForRandom(wireBlackBox)
  def apply(wireBox: WireBox): BitSetGraphForRandom = new BitSetGraphForRandom(MutableBitSetWireBlackBox.from(wireBox))
