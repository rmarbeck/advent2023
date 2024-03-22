import org.apache.pekko
import scala.concurrent.Future
import scala.concurrent.Await
import pekko.util.Timeout
import concurrent.duration.DurationInt

object PekkoLauncher {

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


}
