import scala.annotation.tailrec
import scala.collection
import scala.collection.parallel.*
import scala.collection.parallel.immutable.ParRange
import scala.util.Random
import scala.collection.immutable.{BitSet, HashMap}

given random: Random = new Random

trait Mergeable[A]:
  def nbMerge: Long
  def mergeWith(other: A): A

class MergeableComponent(name: String, val mergingCounter: Long = 0) extends Component(name) with Mergeable[MergeableComponent]:
  override def mergeWith(other: MergeableComponent): MergeableComponent =
    val newName = s"$name-${other.name}"
    MergeableComponent(newName, mergingCounter + other.mergingCounter + 1)
  override def nbMerge: Long = mergingCounter

object MergeableComponent:
  def apply(name: String, mergingCounter: Long): MergeableComponent = new MergeableComponent(name, mergingCounter)
  def apply(component: Component): MergeableComponent = new MergeableComponent(component.name)


class MergeableInt(val value: Int, val mergingCounter: Long = 0) extends Mergeable[MergeableInt]:
  override def mergeWith(other: MergeableInt): MergeableInt =
    val newValue = this.value
    MergeableInt(newValue, mergingCounter + other.mergingCounter + 1)
  override def nbMerge: Long = mergingCounter

  override def toString: String = s"$value ($mergingCounter)"

object MergeableInt:
  def apply(value: Int, mergingCounter: Long): MergeableInt = new MergeableInt(value, mergingCounter)
  def apply(value: Int): MergeableInt = new MergeableInt(value)


trait GraphForRandom[T]:
  def getElements: Seq[T]
  def merge(one: T, other: T): GraphForRandom[T]
  def cutBetweenTwoEdges: Either[String, Long]
  def getConnectedElementsOnEdge(index: Int): (T, T)
  def nbOfEdges: Long


trait WireBlackBox[A <: Mergeable[_]]:
  def getConnectedElementsOnEdge(index: Int): (A, A)
  def getSummits: Seq[A]
  def merge(first: A, second: A): WireBlackBox[A]
  def nbOfEdges: Long

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

case class SimpleWireBlackBox(internalWires: IndexedSeq[Wire[MergeableComponent]]) extends WireBlackBox[MergeableComponent]:
  override def getConnectedElementsOnEdge(index: Int): (MergeableComponent, MergeableComponent) =
    val ends = internalWires(index).ends
    (ends.head, ends.last)
  override lazy val getSummits: Seq[MergeableComponent] = internalWires.flatMap(_.ends).distinct
  override lazy val nbOfEdges: Long = internalWires.size
  override def merge(one: MergeableComponent, other: MergeableComponent): WireBlackBox[MergeableComponent] =
    val mergedComponent = one.mergeWith(other)

    val (toUpdate, uptoDate) = internalWires.partition(wire => wire.ends.contains(one) || wire.ends.contains(other))
    val updatedWires =
      toUpdate.foldLeft(Seq[Wire[MergeableComponent]]()):
        case (acc, wire) if wire.ends.contains(one) && wire.ends.contains(other) => acc
        case (acc, wire) if wire.ends.contains(one) =>
          val otherEnd = wire.ends.filterNot(_ == one).head
          Wire(Set(mergedComponent, otherEnd)) +: acc
        case (acc, wire) =>
          val otherEnd = wire.ends.filterNot(_ == other).head
          Wire(Set(mergedComponent, otherEnd)) +: acc

    SimpleWireBlackBox(uptoDate ++ updatedWires)

object SimpleWireBlackBox:
  def from(wireBox: WireBox): SimpleWireBlackBox = SimpleWireBlackBox.from(wireBox.wires)
  def from(wires: Seq[Wire[_]]) =
    val forInternalUse =
      wires.toIndexedSeq.map:
        wire =>
          val newEnds = wire.ends.map:
            case mergeableComponent: MergeableComponent => mergeableComponent
            case component: Component => MergeableComponent(component)
          Wire(newEnds)

    new SimpleWireBlackBox(forInternalUse)


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


class SimpleGraphForRandom(wireBlackBox: WireBlackBox[MergeableComponent]) extends GraphForRandom[MergeableComponent]:
  override lazy val getElements: Seq[MergeableComponent] = wireBlackBox.getSummits

  override lazy val nbOfEdges: Long = wireBlackBox.nbOfEdges

  override def cutBetweenTwoEdges: Either[String, Long] =
    getElements.size match
      case 2 => Right(nbOfEdges)
      case value => Left(s"Not supported, should have 2 elements and has $value")

  override def getConnectedElementsOnEdge(index: Int): (MergeableComponent, MergeableComponent) = wireBlackBox.getConnectedElementsOnEdge(index)

  override def merge(one: MergeableComponent, other: MergeableComponent) = SimpleGraphForRandom(wireBlackBox.merge(one, other))

object SimpleGraphForRandom:
  def apply(wireBlackBox: WireBlackBox[MergeableComponent]): SimpleGraphForRandom = new SimpleGraphForRandom(wireBlackBox)
  def apply(wireBox: WireBox): SimpleGraphForRandom = new SimpleGraphForRandom(SimpleWireBlackBox.from(wireBox))

def MinCupRandomStep[T <: Mergeable[_]](graph: GraphForRandom[T])(using random: Random): (Long, Long) =
  def cut(subGraph: GraphForRandom[T]): Long = subGraph.cutBetweenTwoEdges.fold(_ => -1l, identity)

  @tailrec
  def findTwoToMerge(graph: GraphForRandom[T]): (Long, Long) =
    graph.getElements.size match
      case 2 =>
        val (first, second) = (graph.getElements.head, graph.getElements.last)
        (first.nbMerge + 1, cut(graph))
      case size if size > 2 =>
        val rank1 = random.nextInt(graph.nbOfEdges.toInt)
        val (first, second) = graph.getConnectedElementsOnEdge(rank1)
        val merged = graph.merge(first, second)
        findTwoToMerge(merged)

  findTwoToMerge(graph)

def MinCutRandom[T <: Mergeable[_]](graph: GraphForRandom[T], target: Int, maxTries: Int): Option[(Long, Long)] =
  val nbProcs = Runtime.getRuntime.availableProcessors()
  val innerLoopTries = nbProcs * 4
  val outerLoopTries = maxTries / innerLoopTries


  val result = (1 to outerLoopTries).iterator.flatMap:
      outerIndex =>
        random.setSeed(outerIndex)
        ParRange(start = 1, end = innerLoopTries, step = 1, inclusive = true).map:
          _ =>
            //val start = System.currentTimeMillis()
            val result = MinCupRandomStep(graph)(using Random)
            //println(s"${System.currentTimeMillis() - start} ms")
            result
        .find:
          (graphSize, cut) => cut <= target
    .nextOption

  result
