object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    import scala.collection.immutable.BitSet
    import scala.collection.immutable.TreeSet

    def part1(input: String): Int =
      val alist = parse(input)
      val g = readGraph(alist)
      val (graph, cut, nbLoops, nbCuts) = minimumCut(g)
      val (out, in) = graph.partition(cut)
      println(s"Nb loops = $nbLoops, and nbCuts = $nbCuts")
      in.size * out.size

    type Id = Int
    type Vertices = BitSet
    type Weight = Map[Id, Map[Id, Int]]

    def parse(input: String): Map[String, Set[String]] =
      input
        .linesIterator
        .map:
          case s"$key: $values" => key -> values.split(" ").toSet
        .toMap

    def readGraph(alist: Map[String, Set[String]]): Graph =
      val all = alist.flatMap((k, vs) => vs + k).toSet

      val (_, lookup) =
        // perfect hashing
        val initial = (0, Map.empty[String, Id])
        all.foldLeft(initial): (acc, s) =>
          val (id, seen) = acc
          (id + 1, seen + (s -> id))

      def asEdges(k: String, v: String) =
        val t = (lookup(k), lookup(v))
        t :: t.swap :: Nil

      val v = lookup.values.to(BitSet)
      val nodes = v.unsorted.map(id => id -> BitSet(id)).toMap
      val edges =
        for
          (k, vs) <- alist.toSet
          v <- vs
          e <- asEdges(k, v)
        yield
          e

      val w = edges
        .groupBy((v, _) => v)
        .view
        .mapValues: m =>
          m
            .groupBy((_, v) => v)
            .view
            .mapValues(_ => 1)
            .toMap
        .toMap
      Graph(v, nodes, w)

    class MostConnected(
                         totalWeights: Map[Id, Int],
                         queue: TreeSet[MostConnected.Entry]
                       ):

      def pop =
        val id = queue.head.id
        id -> MostConnected(totalWeights - id, queue.tail)

      def expand(z: Id, explore: Vertices, w: Weight) =
        val connectedEdges =
          w(z).view.filterKeys(explore)
        var totalWeights0 = totalWeights
        var queue0 = queue
        for (id, w) <- connectedEdges do
          val w1 = totalWeights0.getOrElse(id, 0) + w
          totalWeights0 += id -> w1
          queue0 += MostConnected.Entry(id, w1)
        MostConnected(totalWeights0, queue0)
      end expand

    end MostConnected

    object MostConnected:
      def empty = MostConnected(Map.empty, TreeSet.empty)

      given Ordering[Entry] = (e1, e2) =>
        val first = e2.weight.compareTo(e1.weight)
        if first == 0 then e2.id.compareTo(e1.id) else first

      class Entry(val id: Id, val weight: Int):
        override def hashCode: Int = id

        override def equals(that: Any): Boolean = that match
          case that: Entry @unchecked=> id == that.id
          case _ => false

    case class Graph(v: Vertices, nodes: Map[Id, Vertices], w: Weight):
      def cutOfThePhase(t: Id) = Graph.Cut(t = t, edges = w(t))

      def partition(cut: Graph.Cut): (Vertices, Vertices) =
        (nodes(cut.t), (v - cut.t).flatMap(nodes))

      def shrink(s: Id, t: Id): Graph =
        def fetch(x: Id) =
          w(x).view.filterKeys(y => y != s && y != t)

        val prunedW = (w - t).view.mapValues(_ - t).toMap

        val fromS = fetch(s).toMap
        val fromT = fetch(t).map: (y, w0) =>
          y -> (fromS.getOrElse(y, 0) + w0)
        val mergedWeights = fromS ++ fromT

        val reverseMerged = mergedWeights.view.map: (y, w0) =>
          y -> (prunedW(y) + (s -> w0))

        val v1 = v - t // 5.
        val w1 = prunedW + (s -> mergedWeights) ++ reverseMerged
        val nodes1 = nodes - t + (s -> (nodes(s) ++ nodes(t)))
        Graph(v1, nodes1, w1)
      end shrink

    object Graph:
      def emptyCut = Cut(t = -1, edges = Map.empty)

      case class Cut(t: Id, edges: Map[Id, Int]):
        lazy val weight: Int = edges.values.sum

    def minimumCutPhase(g: Graph, loop: Int): (Graph, Graph.Cut, Int) =
      val a = g.v.head
      var A = a :: Nil
      var explore = g.v - a
      var nbInnerLoops = loop
      var mostConnected =
        MostConnected.empty.expand(a, explore, g.w)
      while explore.nonEmpty do
        nbInnerLoops += 1
        val (z, rest) = mostConnected.pop
        A ::= z
        explore -= z
        mostConnected = rest.expand(z, explore, g.w)
      val t :: s :: _ = A: @unchecked
      (g.shrink(s, t), g.cutOfThePhase(t), nbInnerLoops)

    /** See Stoer-Wagner min cut algorithm
     * https://dl.acm.org/doi/pdf/10.1145/263867.263872
     */
    def minimumCut(g: Graph) =
      var g0 = g
      var nbCutOfThePhase = 0
      var nbCuts = 0
      var min = (g, Graph.emptyCut, 0)
      while g0.v.size > 1 do
        nbCuts += 1
        val (g1, cutOfThePhase, currentLoops) = minimumCutPhase(g0, nbCutOfThePhase)
        nbCutOfThePhase = currentLoops
        if cutOfThePhase.weight < min(1).weight
          || min(1).weight == 0 // initial case
        then
          min = (g0, cutOfThePhase, nbCutOfThePhase, nbCuts)
        g0 = g1
      min

    val result1 = s"${part1(inputLines.mkString("\n"))}"
    val result2 = s""

    (s"$result1", s"$result2")

end Solution

