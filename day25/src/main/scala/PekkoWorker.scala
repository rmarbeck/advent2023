
import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef, ActorSystem, PostStop, DispatcherSelector}
import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success


import scala.util.Random

object Computer:
  import ComputingMessages.*

  def apply(index: Int): Behavior[Computation] =
    given random: Random = new Random
    random.setSeed(index)
    manage(index)

  private def manage(index: Int)(using Random): Behavior[Computation] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[Computation] {
        case Mission(wireBox, target, sender) =>
          context.log.debug(s"Computer[$index] Starting mission from with target {}", target)
          context.self ! Continue()
          retry(wireBox, target, sender, index)
        case Finish() =>
          context.log.debug("[Idle] : No need to finish, not working.......")
          Behaviors.same
        case Continue() =>
          context.log.debug("[Idle] : Receiving continue, don't take it into account")
          Behaviors.same
        case ResultFound(result) =>
          context.log.debug("[Idle] : Receiving good result, don't take it into account")
          Behaviors.same
      }.receiveSignal {
        case (context, PostStop) =>
          context.log.debug("Computation stopped")
          Behaviors.same
        case other =>
          context.log.debug(s"Receiving a signal $other")
          Behaviors.same
      }
    }

  private def retry(wireBox: WireBox, target: Int, requester: ActorRef[ComputationResult], index: Int)(using Random): Behavior[Computation] =
    Behaviors.receive[Computation] { (context, message) =>
      context.log.trace("[Retry] : Receiving message....... {}", message)

      message match
        case Continue() =>
          context.log.debug("[Retry] : Receiving Continue()")

          given executionContext: ExecutionContext =
            context.system.dispatchers.lookup(DispatcherSelector.fromConfig("for-blocking-dispatcher"))

          random.setSeed(random.nextInt())
          doRunMission(wireBox, target, index).andThen:
            case Success(Some(value: Int)) => context.self ! ResultFound(value)
            case Success(None) => context.self ! Continue()
            case _ =>
              context.log.error("[Retry] : Error in retry, finishing")
              context.self ! Finish()

          Behaviors.same

        case ResultFound(result) =>
          requester ! Successful(result)
          manage(index)

        case Finish() =>
          context.log.debug("[Retry] : Finishing.......")
          manage(index)

        case _ =>
          context.log.error("[Idle] : Unmanaged message received : {}", message.getClass)
          Behaviors.unhandled
    }

  private def doRunMission(wirebox: WireBox, target: Int, index: Int)(using random: Random, executionContext: ExecutionContext): Future[Option[Int]] =
    Future {
      MinCupRandomStep(SimpleGraphForRandom(wirebox))(using Random) match
        case (_, valueOfCut) if valueOfCut > target => None
        case (goal, _) => Some(goal.toInt)
    }