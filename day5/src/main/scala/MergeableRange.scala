

trait MergeableRange[T](using ordering: Ordering[T]):
  def startsBefore(other: MergeableRange[T]): Boolean = compareStarts(other) <= 0
  def startsAfter(other: MergeableRange[T]): Boolean = compareStarts(other) >= 0
  def startsStrictlyBefore(other: MergeableRange[T]): Boolean = compareStarts(other) < 0
  def startsStrictlyAfter(other: MergeableRange[T]): Boolean = compareStarts(other) > 0
  def startsTogether(other: MergeableRange[T]): Boolean = compareStarts(other) == 0
  def endsBefore(other: MergeableRange[T]): Boolean = compareEnds(other) <= 0
  def endsAfter(other: MergeableRange[T]): Boolean = compareEnds(other) >= 0
  def endsStrictlyBefore(other: MergeableRange[T]): Boolean = compareEnds(other) < 0
  def endsStrictlyAfter(other: MergeableRange[T]): Boolean = compareEnds(other) > 0
  def endsTogether(other: MergeableRange[T]): Boolean = compareEnds(other) == 0

  def endsStrictlyBeforeStartOf(other: MergeableRange[T]): Boolean = compareEndAndStartOf(other) < 0
  def startsStrictlyAfterEndOf(other: MergeableRange[T]): Boolean = compareStartAndEndOf(other) > 0

  def overlap(other: MergeableRange[T]): Boolean = ! (startsStrictlyBefore(other) && endsStrictlyBefore(other) || startsStrictlyAfter(other) && endsStrictlyAfter(other))
  def includes(other: MergeableRange[T]): Boolean = other.isIncludedIn(this)
  def isIncludedIn(other: MergeableRange[T]): Boolean = overlap(other) && startsAfter(other) && endsBefore(other)

  def compareStarts[A >: MergeableRange[T]](other: A): Int
  def compareEnds(other: MergeableRange[T]): Int
  def compareStartAndEndOf(other: MergeableRange[T]): Int
  def compareEndAndStartOf(other: MergeableRange[T]): Int


class IncreasingMRange(val start: Long, val end: Long) extends MergeableRange[Long]:
  def compareStarts[IncreasingMRange](other: IncreasingMRange): Int =
    start.compareTo(other.start)
  def compareEnds(other: IncreasingMRange): Int = end.compareTo(other.end)
  def compareStartAndEndOf(other: IncreasingMRange): Int = start.compareTo(other.end)
  def compareEndAndStartOf(other: IncreasingMRange): Int = end.compareTo(other.start)

  override def compareEnds(other: MergeableRange[Long]): Int = ???

  override def compareStartAndEndOf(other: MergeableRange[Long]): Int = ???

  override def compareEndAndStartOf(other: MergeableRange[Long]): Int = ???




