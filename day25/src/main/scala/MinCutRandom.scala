import scala.annotation.tailrec
import scala.collection.parallel.*
import scala.collection.parallel.immutable.ParRange
import scala.util.Random

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

class SimpleGraphForRandom(wireBox: WireBox) extends GraphForRandom[MergeableComponent]:
  override lazy val getElements: Seq[MergeableComponent] =
    wireBox.wires.flatMap(_.ends).distinct.map:
      case mergeableComponent: MergeableComponent => mergeableComponent
      case component: Component => MergeableComponent(component)

  override lazy val nbOfEdges: Long = wireBox.wires.size

  override def cutBetweenTwoEdges: Either[String, Long] =
    getElements.size match
      case 2 => Right(nbOfEdges)
      case value => Left(s"Not supported, should have 2 elements and has $value")

  override def getConnectedElementsOnEdge(index: Int): (MergeableComponent, MergeableComponent) =
    val ends = wireBox.wires(index).ends.map:
      case mergeableComponent: MergeableComponent => mergeableComponent
      case component: Component => MergeableComponent(component)
    (ends.head, ends.last)

  override def merge(one: MergeableComponent, other: MergeableComponent): GraphForRandom[MergeableComponent] =
    val mergedComponent = one.mergeWith(other)
    val updatedWires =
      wireBox.wires.withFilter:
        wire => !(wire.ends.contains(one) && wire.ends.contains(other))
      .map:
        case wire if wire.ends.contains(one) =>
          val otherEnd = wire.ends.filterNot(_ == one).head
          Wire(Set(mergedComponent, otherEnd))
        case wire if wire.ends.contains(other) =>
          val otherEnd = wire.ends.filterNot(_ == other).head
          Wire(Set(mergedComponent, otherEnd))
        case wire => wire

    SimpleGraphForRandom(WireBox(updatedWires))

def MinCupRandomStep[T <: Mergeable[_]](graph: GraphForRandom[T])(using random: Random): (Long, Long) =
  def cut(subGraph: GraphForRandom[T]): Long = subGraph.cutBetweenTwoEdges.fold(_ => -1l, identity)

  def findTwoToMerge(graph: GraphForRandom[T]): (Long, Long) =
    graph.getElements.size match
      case 2 =>
        val (first, second) = (graph.getElements.head, graph.getElements.last)
        (first.nbMerge + 1, cut(graph))
      case size if size > 2 =>
        val rank1 = random.nextInt(graph.nbOfEdges.toInt)
        val (first, second) = graph.getConnectedElementsOnEdge(rank1)
        findTwoToMerge(graph.merge(first, second))

  findTwoToMerge(graph)

def MinCutRandom[T <: Mergeable[_]](graph: GraphForRandom[T], target: Int, maxTries: Int): Option[(Long, Long)] =
  val nbProcs = Runtime.getRuntime.availableProcessors()
  val innerLoopTries = nbProcs * 4
  val outerLoopTries = maxTries / innerLoopTries

  val result =
    (1 to outerLoopTries).iterator.flatMap:
      outerIndex =>
        random.setSeed(outerIndex)
        ParRange(start = 1, end = innerLoopTries, step = 1, inclusive = true).map:
          _ => MinCupRandomStep(graph)(using Random)
        .find:
          (graphSize, cut) => cut <= target
    .nextOption

  result
