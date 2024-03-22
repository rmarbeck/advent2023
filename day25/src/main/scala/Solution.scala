
import scala.concurrent.Future
import scala.concurrent.Await
import org.apache.pekko
import pekko.util.Timeout
import concurrent.duration.DurationInt

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val wirebox = WireBox.from(inputLines)

    /*val maxRandomTries = 3000
    val nbOfCuts = 3

    val searchResult = MinCutRandom(BitSetGraphForRandom(wirebox), nbOfCuts, maxRandomTries)

    val resultPart1 = searchResult match
      case Some(nbOnOneSide, _) =>
        val nbOnOtherSide = wirebox.nbOfEdges - nbOnOneSide
        nbOnOtherSide * nbOnOneSide
      case _ => throw Exception("Not found")*/

    import concurrent.ExecutionContext.Implicits.global
    val futureResultPart1 = runThroughPekko(wirebox)(using timeout = 120.seconds).map:
      nbOnOneSide =>
        val nbOnOtherSide = wirebox.nbOfEdges - nbOnOneSide
        nbOnOtherSide * nbOnOneSide

    val resultPart1 =
      try {
        Await.result(futureResultPart1, 110.seconds).toString
      } catch
        case exception: Exception => "Not found within time limit"


    val result1 = s"${resultPart1}"
    val result2 = s"Happy Christmas"

    (s"${result1}", s"${result2}")

end Solution

def runThroughPekko(wirebox: WireBox)(using timeout: Timeout): Future[Int] =

  import pekko.actor.typed.{ActorRef, ActorSystem}
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import scala.concurrent.ExecutionContext
  import scala.concurrent

  val system: ActorSystem[Messages.Command] = ActorSystem(PekkoRoot(), "rootsolver")
  given implicitSystem: ActorSystem[_] = system

  val nbProcs = Runtime.getRuntime.availableProcessors()
  system ! Messages.Start(nbProcs)

  import concurrent.ExecutionContext.Implicits.global

  val eventualComputer: Future[Messages.ResultFromComputer] = {
    system.ask(sender => Messages.Solve(wirebox, 3, sender))
  }

  eventualComputer.andThen {
    case _ =>
      val start = System.currentTimeMillis()
      loggerAOC.debug(s"Closing Pekko")
      system ! Messages.Stop()
      system.terminate()
      try {
        Await.ready(system.whenTerminated, 10.seconds)
      } catch
        case exception: Exception => loggerAOC.error(s"Error occured when closing Pekko $exception")
      loggerAOC.debug(s"Pekko closed in ${System.currentTimeMillis() - start} ms")
  }

  eventualComputer.map:
    value => value.result.getValue



case class WireBox(wires: Seq[Wire[_]]):
  def nbOfEdges = wires.flatMap(_.ends).distinct.size

object WireBox:
  def from(input: Seq[String]): WireBox =
    def from(singleInput: String): List[Wire[_]] =
      singleInput match
        case s"$first: $others" =>
          val firstComponent = Component(first)
          for
            other <- others.split(" ").toList
            otherComponent = Component(other)
          yield
            Wire(Set(firstComponent, otherComponent))

    WireBox(input.map(from).flatten)

case class Component(name: String)

case class Wire[A <: Component](ends: Set[A]):
  require(ends.size == 2)
  private lazy val endsIndexed = ends.toIndexedSeq

  def otherThan(component: A): Option[A] =
    endsIndexed.indexOf(component) match
      case -1 => None
      case 0 => Some(endsIndexed(1))
      case 1 => Some(endsIndexed(0))

  override def toString: String = ends.mkString(",")


