import java.awt.Component

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val wirebox = WireBox.from(inputLines)


    val resultPar1 = runThroughPekko(wirebox)


    /*val maxRandomTries = 3000
    val nbOfCuts = 3

    val searchResult = MinCutRandom(SimpleGraphForRandom(wirebox), nbOfCuts, maxRandomTries)

    val resultPar1 = searchResult match
      case Some(nbOnOneSide, _) =>
        val nbOnOtherSide = wirebox.nbOfEdges - nbOnOneSide
        nbOnOtherSide * nbOnOneSide
      case _ => throw Exception("Not found")*/


    val result1 = s"$resultPar1"
    val result2 = s""

    (s"${result1}", s"${result2}")

end Solution

def runThroughPekko(wirebox: WireBox) =
  import org.apache.pekko
  import pekko.util.Timeout
  import concurrent.duration.DurationInt
  import pekko.actor.typed.{Behavior, ActorRef, ActorSystem, PostStop}
  import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}
  import scala.concurrent.Await
  import scala.concurrent.Future
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.Askable
  import org.apache.pekko.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import scala.util.Failure
  import scala.util.Success
  import math.Numeric.Implicits.infixNumericOps
  import math.Integral.Implicits.infixIntegralOps

  given timeout: Timeout = 60.seconds

  val system: ActorSystem[PekkoRoot.Command] = ActorSystem(PekkoRoot(), "rootsolver")
  given implicitSystem: ActorSystem[_] = system

  val nbProcs = Runtime.getRuntime.availableProcessors()

  system ! PekkoRoot.Start(nbProcs)

  val eventualComputer: Future[PekkoRoot.ResultFromComputer] = system.ask(sender => PekkoRoot.Solve(wirebox, 3, sender))

  eventualComputer.onComplete {
    case Success(PekkoRoot.ResultFromComputer(value)) => println(s"[[[[[[[[  Yay, $value is the value!  ]]]]]]]]]")
    case Failure(ex) => println(s"Boo! didn't get the right value: ${ex.getMessage}")
  }(system.executionContext)


  val resultOfComputation = Await.ready(eventualComputer, 14.seconds).value match
    case Some(Success(response)) => response.result.getValue
    case Some(Failure(e)) =>
      println("Failure detected")
      0
    case _ =>
      println("Other case")
      -1

  system ! PekkoRoot.Stop()
  system.terminate()

  Await.ready(system.whenTerminated, 10.seconds)
  resultOfComputation

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


