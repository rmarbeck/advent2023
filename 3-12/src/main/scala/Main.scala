import scala.collection.immutable.Seq
import scala.io.Source

@main def hello: Unit =
  println("Launching 3-12")
  val bufferedSource = Source.fromFile("./src/main/resources/test1.txt")
  val data = bufferedSource.getLines().toSeq
  data.zipWithIndex.foreach((line, index) => ElementContainer.addLine(line, index))
  val result1 = ElementContainer.figures.filter(figure => ElementContainer.symbols.find(_.isContiguous(figure)).isDefined).map(_.value).sum
  val result2 = ElementContainer.symbols.filter(_.value == '*').map(_.getContiguousFigures(ElementContainer.figures)).map:
    case Some(figure1, figure2) => figure1.value * figure2.value
    case None => 0
  .sum
  println(s"1 : ${result1}")
  println(s"2 : ${result2}")

object ElementContainer:
  var figures: Seq[Figure] = Nil
  var symbols: Seq[Symbol] = Nil

  def addFigure(figureToAdd: Figure) =
    def merge(figure1: Figure, figure2: Figure) =
      Figure(figure1.value*10+figure2.value, figure1.line, figure1.startCol, figure2.startCol)
    //println(s"adding ${figureToAdd} in $figures")
    val existingOne = figures.find(existingPartialFigure => existingPartialFigure.line == figureToAdd.line && existingPartialFigure.endCol == figureToAdd.startCol-1)
    figures = existingOne match
      case Some(figure) => figures.filterNot(_.equals(figure)) :+ merge(figure, figureToAdd)
      case _ => figures :+ figureToAdd

  def addSymbol(symbolToAdd: Symbol) =
    symbols = symbols :+ symbolToAdd
    //println(s"adding ${symbolToAdd} in $symbols")

  def addLine(line: String, lineNum: Int): Unit =
    line.zipWithIndex.map:
      case ('.', _) => None
      case (c, i) if c.isDigit => Some(Figure(c.asDigit, lineNum, i, i))
      case (c, i) => Some(Symbol(c, lineNum, i))
    .foreach:
      case Some(figure: Figure) => addFigure(figure)
      case Some(symbol: Symbol) => addSymbol(symbol)
      case _ => ()

sealed class Element
case class Figure(value: Int, line: Int, startCol: Int, endCol: Int) extends Element
case class Symbol(value: Char, line: Int, symbolCol: Int) extends Element:
  def getContiguousFigures(figures: Seq[Figure]): Option[(Figure, Figure)] =
    if (value == '*')
      val values = figures.filter(isContiguous(_))
      values.length match
        case 2 => Some(values(0), values(1))
        case _ => None
    else
      None
  def isContiguous(figure: Figure): Boolean =
    if (figure.line == line || figure.line == line + 1 || figure.line == line - 1)
      if (figure.line == line)
        figure.startCol == symbolCol + 1 || figure.endCol == symbolCol - 1
      else
        symbolCol >= figure.startCol - 1 && symbolCol <= figure.endCol + 1
    else
      false
