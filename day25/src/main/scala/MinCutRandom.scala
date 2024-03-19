import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.parallel.*
import collection.parallel.CollectionConverters.SetIsParallelizable
import collection.parallel.CollectionConverters.seqIsParallelizable
import collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.util.Random

given random: Random = new Random

class MergeableComponent(name: String, val mergingCounter: Long = 0) extends Component(name) with Mergeable[Component, MergeableComponent]:
  def mergeWith(mergeableComponent: MergeableComponent): MergeableComponent =
    val newName = s"${this.name}-${mergeableComponent.name}"
    val (counter1, counter2) = (this.mergingCounter, mergeableComponent.mergingCounter)
    MergeableComponent(newName, counter1 + counter2 + 1)
  override def nbMerge: Long = mergingCounter

class SimpleGraphForRandom(wireBox: WireBox) extends GraphForRandom[MergeableComponent]:
  override lazy val getElements: Seq[MergeableComponent] = wireBox.wires.flatMap(_.ends).distinct.map(component => MergeableComponent(component.name))
  override lazy val nbOfEdges: Long = wireBox.wires.size * 2

  override def getNeighboursOfIn(first: MergeableComponent, second: MergeableComponent): Int = wireBox.wires.size

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

trait Mergeable[A, B]:
  def nbMerge: Long
  def mergeWith(other: B): B

trait GraphForRandom[T]:
  def getElements: Seq[T]
  def merge(one: T, other: T): GraphForRandom[T]
  def getNeighboursOfIn(first: T, second: T): Int
  def nbOfEdges: Long


def MinCupRandomStep[T <: Mergeable[_, _]](graph: GraphForRandom[T])(using random: Random): (Long, Long) =
  def cutBetween(first: T, second: T, subGraph: GraphForRandom[T]): Long = subGraph.getNeighboursOfIn(first, second)

  def findTwoToMerge(graph: GraphForRandom[T]): (Long, Long) =
    graph.getElements.size match
      case 2 =>
        val (first, second) = (graph.getElements(0), graph.getElements(1))
        //println(s"$first")
        //println(s"***************** ${cutBetween(first, second, graph)}")
        //println(s"$second")
        (first.nbMerge, cutBetween(first, second, graph))
      case size if size > 2 =>
        val rank1 = random.nextInt(size)
        val rank2 = rank1 * 2 % size match
          case value if value == rank1 => (rank1 + 1) % size
          case initial => initial
        val (first, second) = (graph.getElements(rank1), graph.getElements(rank2))
        findTwoToMerge(graph.merge(first, second))

  findTwoToMerge(graph)

def MinCutRandom[T <: Mergeable[_, _]](graph: GraphForRandom[T], target: Int, maxTries: Int): Option[(Long, Long)] =
  def solve(graph: GraphForRandom[T], bestCutGraphSize: Long, cutValue: Long)(using Random): (Long, Long) =
    MinCupRandomStep(graph)(using Random)

  random.setSeed(10)
  val sizeOfInitialGraph = graph.getElements.size

  val result = (1 to maxTries).par.map:
     _=> solve(graph, sizeOfInitialGraph, graph.nbOfEdges)(using Random)
  .find:
    (graphSize, cut) => cut <= target

  println(result)
  result
