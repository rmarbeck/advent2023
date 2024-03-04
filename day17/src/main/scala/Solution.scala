val maxStepsInOneDirection = 3
val maxStepsInOneDirectionPart2 = 10
val minStepsInOneDirectionPart2 = 4
object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val (height: Int, width: Int) = (inputLines.length, inputLines(0).length)
    val resultPart1 = {
      given heatMap: HeatMap = HeatMap(inputLines)

      val summits = heatMap.getSummits

      val graphPart1 = GraphFromArray(summits)(nextPart1)
      val startPart1 = summits.find:
        case Summit(0, 0, _, _, 0) => true
        case _ => false
      .get

      val endPart1 = summits.toList.filter:
        case Summit(row, col, _, Up2Down | Left2Right, _) if row == height - 1 && col == width - 1 => true
        case _ => false

      Dijkstra.solve(graphPart1, startPart1, endPart1).drop(1).map(_.value).sum
    }

    val resultPart2 = {
      given heatMapPart2: HeatMap = HeatMap(inputLines)

      val summitsPart2 = heatMapPart2.getSummitsPart2
      val startPart2 = summitsPart2.find:
        case Summit(0, 0, _, _, 0) => true
        case _ => false
      .get

      val endPart2 = summitsPart2.toList.filter:
        case Summit(row, col, _, Up2Down | Left2Right, lastDirCounter) if row == height - 1 && col == width - 1 && lastDirCounter >= minStepsInOneDirectionPart2 => true
        case _ => false

      val graphPart2 = GraphFromArray(summitsPart2)(nextPart2)
      Dijkstra.solve(graphPart2, startPart2, endPart2).drop(1).map(_.value).sum
    }

    val result1 = s"$resultPart1"
    val result2 = s"$resultPart2"

    (s"${result1}", s"${result2}")

  def nextPart1(summit: Summit)(using heatMap: HeatMap): List[Summit] =
    val (currentRow, currentCol) = (summit.row, summit.col)
    Direction.values.toList.filterNot(_ == summit.lastDir.opposite).flatMap:
      currentDir =>
        val (nextRow, nextCol) = currentDir match
          case Left2Right => (currentRow, currentCol + 1)
          case Right2Left => (currentRow, currentCol - 1)
          case Up2Down => (currentRow + 1, currentCol)
          case Down2Up => (currentRow - 1, currentCol)

        (heatMap.isDefinedAt(nextRow, nextCol), summit.lastDir == currentDir) match
          case (false, _) => None
          case (true, true) =>
            summit.lastDirCounter match
              case value if value == maxStepsInOneDirection => None
              case value => Some(summit.copy(row = nextRow, col = nextCol, value = heatMap.getHeat(nextRow, nextCol), lastDirCounter = value + 1))
          case (true, false) => Some(summit.copy(row = nextRow, col = nextCol, value = heatMap.getHeat(nextRow, nextCol), lastDir = currentDir, lastDirCounter = 1))

  def nextPart2(summit: Summit)(using heatMap: HeatMap): List[Summit] =
    val (currentRow, currentCol) = (summit.row, summit.col)
    Direction.values.toList.filterNot(_ == summit.lastDir.opposite).flatMap:
      currentDir =>
        val (nextRow, nextCol) = currentDir match
          case Left2Right => (currentRow, currentCol + 1)
          case Right2Left => (currentRow, currentCol - 1)
          case Up2Down => (currentRow + 1, currentCol)
          case Down2Up => (currentRow - 1, currentCol)

        (heatMap.isDefinedAt(nextRow, nextCol), summit.lastDir == currentDir) match
          case (false, _) => None
          case (true, true) =>
            summit.lastDirCounter match
              case value if value == maxStepsInOneDirectionPart2 => None
              case value => Some(summit.copy(row = nextRow, col = nextCol, value = heatMap.getHeat(nextRow, nextCol), lastDirCounter = value + 1))
          case (true, false) =>
            summit.lastDirCounter match
              case value if value < minStepsInOneDirectionPart2 => None
              case value => Some(summit.copy(row = nextRow, col = nextCol, value = heatMap.getHeat(nextRow, nextCol), lastDir = currentDir, lastDirCounter = 1))

end Solution

case class HeatMap(inputLines: Seq[String]):
  val (height: Int, width: Int) = (inputLines.length, inputLines(0).length)
  val data = inputLines.map(_.toCharArray.map(_.asDigit)).toArray

  lazy val getSummits: Seq[Summit] =
    (for
      row <- 0 until height
      col <- 0 until width
    yield
      allSummitsFrom(row, col, data(row)(col))
    ).flatten

  lazy val getSummitsPart2: Seq[Summit] =
    (for
      row <- 0 until height
      col <- 0 until width
    yield
      allSummitsFromPart2(row, col, data(row)(col))
      ).flatten

  def isDefinedAt(row: Int, col: Int): Boolean = (row >= 0 && row < height) && (col >= 0 && col < width)
  def getHeat(row: Int, col: Int): Int = data(row)(col)

  def allSummitsFrom(row: Int, col: Int, value: Int): List[Summit] =
    Direction.values.toList.flatMap:
      currentDir =>
        (0 to maxStepsInOneDirection).map:
          step => Summit(row, col, value, currentDir, step)

  def allSummitsFromPart2(row: Int, col: Int, value: Int): List[Summit] =
    Direction.values.toList.flatMap:
      currentDir =>
        (0 to maxStepsInOneDirectionPart2).map:
          step => Summit(row, col, value, currentDir, step)
