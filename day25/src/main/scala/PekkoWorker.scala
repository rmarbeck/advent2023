
import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef, ActorSystem, PostStop}
import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}

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
          context.log.info(s"Computer[$index] Starting mission from with target {}", target)
          context.self ! Continue()
          retry(wireBox, target, sender, index)
        case Finish() =>
          context.log.info("[Idle] : No need to finish, not working.......")
          Behaviors.same
        case Continue() =>
          context.log.debug("[Idle] : Receiving continuer, don't take it into account")
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
      context.log.debug("[Retry] : Receiving message....... {}", message)

      message match
        case Continue() =>
          context.log.info("[Retry] : Receiving message....... {}", message)
          random.setSeed(random.nextInt())

          doRunMission(wireBox, target, index) match
            case Some(validResponse) =>
              context.log.info("[Retry] : Solution found")
              requester ! Successful(validResponse)
              manage(index)
            case None =>
              context.self ! Continue()
              Behaviors.same

        case Finish() =>
          context.log.info("[Retry] : Receiving message....... {}", message)
          context.log.debug("[Retry] : Finishing.......")
          manage(index)

        case _ => Behaviors.unhandled
    }

  def doRunMission(wirebox: WireBox, target: Int, index: Int)(using random: Random): Option[Int] =
    MinCupRandomStep(SimpleGraphForRandom(wirebox))(using Random) match
      case (_, valueOfCut) if valueOfCut > target => None
      case (goal, _) => Some(goal.toInt)