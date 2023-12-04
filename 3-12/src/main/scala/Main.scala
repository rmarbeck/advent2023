import scala.collection.immutable.Seq
import scala.io.Source

@main def hello: Unit =
  println("Launching 3-12")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val data = bufferedSource.getLines().toSeq
  data.zipWithIndex.foreach((line, index) => ElementContainer.addLine(line, index))
  val result1 = ElementContainer.figures.filter(figure => ElementContainer.symbols.find(_.isContiguous(figure)).isDefined).map(_.value).sum
  val result2 = ElementContainer.symbols.filter(_.value == '*').map(_.getGearRatio(ElementContainer.figures)).sum
  println(s"1 : ${result1}")
  println(s"2 : ${result2}")

object ElementContainer:
  var figures: Seq[Figure] = Nil
  var symbols: Seq[Symbol] = Nil

  private def addFigure(figureToAdd: Figure): Unit =
    def merge(figure1: Figure, figure2: Figure) =
      Figure(figure1.value*10+figure2.value, figure1.line, figure1.startCol)
    val existingOne = figures.find(existingPartialFigure => existingPartialFigure.line == figureToAdd.line && existingPartialFigure.endCol == figureToAdd.startCol-1)
    figures = existingOne match
      case Some(figure) => figures.filterNot(_.equals(figure)) :+ merge(figure, figureToAdd)
      case _ => figures :+ figureToAdd

  private def   addSymbol(symbolToAdd: Symbol): Unit =
    symbols = symbols :+ symbolToAdd

  def addLine(line: String, lineNum: Int): Unit =
    line.zipWithIndex.map:
      case ('.', _) => None
      case (c, i) if c.isDigit => Some(Figure(c.asDigit, lineNum, i))
      case (c, i) => Some(Symbol(c, lineNum, i))
    .foreach:
      case Some(figure: Figure) => addFigure(figure)
      case Some(symbol: Symbol) => addSymbol(symbol)
      case _ => ()

sealed class Element
case class Figure(value: Int, line: Int, startCol: Int) extends Element:
  def endCol = startCol + value.toString.length - 1
case class Symbol(value: Char, line: Int, symbolCol: Int) extends Element:
  def getGearRatio(figures: Seq[Figure]): Int =
    getContiguousFigures(figures).map((figure1, figure2) => figure1.value*figure2.value).getOrElse(0)
  def getContiguousFigures(figures: Seq[Figure]): Option[(Figure, Figure)] =
    value match
      case '*' =>
        figures.filter(isContiguous(_)) match
          case Seq(figure1: Figure, figure2: Figure) => Some(figure1, figure2)
          case _ => None
      case _ => None
  def isContiguous(figure: Figure): Boolean =
    def isSameLine =
      figure.line == line
    def isContiguousLine =
      figure.line == line + 1 || figure.line == line - 1
    if (isSameLine || isContiguousLine)
      if (isSameLine)
        figure.startCol == symbolCol + 1 || figure.endCol == symbolCol - 1
      else
        symbolCol >= figure.startCol - 1 && symbolCol <= figure.endCol + 1
    else
      false
