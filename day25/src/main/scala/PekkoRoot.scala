
import org.apache.pekko
import pekko.actor.typed.ActorRef
import pekko.actor.typed.ActorSystem
import pekko.actor.typed.Behavior
import pekko.actor.typed.scaladsl.Behaviors
import pekko.actor.typed.scaladsl.ActorContext

import scala.collection.mutable.{ArrayBuffer, Seq}

object ComputingMessages:
  sealed trait Computation
  final case class Mission(wireBox: WireBox, target: Int, sender: ActorRef[ComputationResult]) extends Computation
  final case class Finish() extends Computation
  final case class Continue() extends Computation

  sealed trait ComputationResult:
    def getValue = 0
  final case class Successful(value: Int) extends ComputationResult:
    override def getValue: Int = value

export ComputingMessages.*


object PekkoRoot:
  sealed trait Command
  final case class Start(max: Int) extends Command
  final case class Stop() extends Command
  final case class Solve(wireBox: WireBox, target: Int, requester: ActorRef[ResultFromComputer]) extends Command
  final case class ResultFromComputer(result: ComputingMessages.ComputationResult) extends Command

  def apply(): Behavior[Command] =
    idle()

  private def idle(): Behavior[Command] =
    Behaviors.setup[Command] { context =>
      Behaviors.receiveMessage { message =>
        context.log.debug("[Idle] : Receiving message....... {}", message)

        message match
          case Start(max) =>
            val children =  ArrayBuffer[ActorRef[Computation]]()
            context.log.debug("[Idle] : Starting children from {} to {}", children.length, max)
            for i <- 1 to max
              do {
                children += context.spawn(Computer(i), s"computer_$i")
              }
            context.log.debug("[Idle] : {} children created", children.length)
            readyToResolve(children)
          case _ => Behaviors.unhandled
      }
    }

  private def readyToResolve(children: ArrayBuffer[ActorRef[Computation]]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      context.log.debug("[Ready] : Receiving message....... {}", message.getClass)

      given ActorContext[Command] = context

      message match
        case Solve(wireBox, target, sender) =>
          val backendResponseMapper: ActorRef[ComputationResult] =
            context.messageAdapter(rsp => ResultFromComputer(rsp))

          solve(wireBox, target, children, backendResponseMapper)
          waitingFirst(children, sender, None)
        case Stop() =>
          stopChildren(children)
          idle()
        case _ => Behaviors.unhandled
    }

  private def waitingFirst(children: ArrayBuffer[ActorRef[Computation]], requester: ActorRef[ResultFromComputer], result: Option[ResultFromComputer]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      context.log.debug("[WaitingFirst] : Receiving message....... {}", message.getClass)

      given ActorContext[Command] = context

      message match
        case ResultFromComputer(value) =>
          context.log.debug("[WaitingFirst] : Value is received {}", value)
          requester ! ResultFromComputer(value)
          askChildrenToFinish(children)
          readyToResolve(children)
        case Stop() =>
          stopChildren(children)
          idle()
        case _ => Behaviors.unhandled
    }

  private def solve(wireBox: WireBox, target: Int, currentChildren: ArrayBuffer[ActorRef[Computation]], sender: ActorRef[ComputationResult])(using context: ActorContext[Command]): Unit =
    context.log.debug("About to send mission with target {}", target)
    currentChildren.foreach(_ ! Mission(wireBox, target, sender))

  private def askChildrenToFinish(currentChildren: ArrayBuffer[ActorRef[Computation]])(using context: ActorContext[Command]): Unit =
    currentChildren.foreach(current => {
      current ! Finish()
    })
  
  
  private def stopChildren(currentChildren: ArrayBuffer[ActorRef[Computation]])(using context: ActorContext[Command]): Unit =
    val children = currentChildren.clone()
    if currentChildren.nonEmpty then
      currentChildren.foreach(current => {
        context.stop(current)
        children -= current
      })
    context.log.debug("************************ {} children after stopping", currentChildren.length)