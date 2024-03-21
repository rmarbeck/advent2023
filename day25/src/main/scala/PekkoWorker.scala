
import PekkoRoot.ResultFromComputer
import org.apache.pekko
import pekko.actor.typed.{Behavior, ActorRef, ActorSystem, PostStop}
import pekko.actor.typed.scaladsl.{Behaviors, ActorContext}

import scala.util.Random

given random: Random = new Random

object Computer:
  import ComputingMessages.*

  def apply(index: Int)(using Random): Behavior[Computation] = manage(index)

  private def manage(index: Int)(using Random): Behavior[Computation] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[Computation] {
        case Mission(wireBox, target, sender) =>
          context.log.debug(s"Computer[$index] Starting mission from with target {}", target)
          solve(wireBox, target, sender, index)
        case Finish() =>
          context.log.debug("[Idle] : No need to finish, not working.......")
          Behaviors.same
        case _ => Behaviors.unhandled
      }.receiveSignal {
        case (context, PostStop) =>
          context.log.debug("Computation stopped")
          Behaviors.same
        case other =>
          context.log.debug(s"Receiving a signal $other")
          Behaviors.same
      }
    }

  private def solve(wireBox: WireBox, target: Int, requester: ActorRef[ComputationResult], index: Int)(using Random): Behavior[Computation] =
    Behaviors.setup[Computation] { context =>
      context.log.debug("[Ready] : Solving....... ")

      given ActorContext[Computation] = context

      doRunMission(wireBox, target, index) match
        case Some(validResponse) =>
          requester ! Successful(validResponse)
          manage(index)
        case None =>
          context.self ! Continue()
          retry(wireBox, target, requester, index)
    }

  private def retry(wireBox: WireBox, target: Int, requester: ActorRef[ComputationResult], index: Int)(using Random): Behavior[Computation] =
    Behaviors.receive[Computation] { (context, message) =>
      context.log.debug("[Retry] : Receiving message....... {}", message)

      message match
        case Continue() =>
          /*val backendResponseMapper: ActorRef[ComputationResult] =
            context.messageAdapter(rsp => ResultFromComputer(rsp))*/

          doRunMission(wireBox, target, index) match
            case Some(validResponse) =>
              requester ! Successful(validResponse)
              manage(index)
            case None =>
              context.self ! Continue()
              Behaviors.same

        case Finish() =>
          context.log.debug("[Retry] : Finishing.......")
          manage(index)

        case _ => Behaviors.unhandled
    }

  def doRunMission(wirebox: WireBox, target: Int, index: Int)(using random: Random): Option[Int] =
    random.setSeed(index)
    MinCupRandomStep(SimpleGraphForRandom(wirebox))(using Random) match
      case (_, valueOfCut) if valueOfCut > target => None
      case (goal, _) => Some(goal.toInt)