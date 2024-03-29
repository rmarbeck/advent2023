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
