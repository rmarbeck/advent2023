
import org.apache.pekko
import pekko.actor.typed.ActorRef
import pekko.actor.typed.ActorSystem
import pekko.actor.typed.Behavior
import pekko.actor.typed.scaladsl.Behaviors
import pekko.actor.typed.scaladsl.ActorContext

object ComputingMessages:
  sealed trait Computation
  final case class Mission(wireBox: WireBox, target: Int, sender: ActorRef[ComputationResult]) extends Computation
  final case class Finish() extends Computation
  final case class ResultFound(result: Int) extends Computation
  final case class Continue() extends Computation

  sealed trait ComputationResult:
    def getValue = 0
  final case class Successful(value: Int) extends ComputationResult:
    override def getValue: Int = value

export ComputingMessages.*


object Messages:
  sealed trait Command
  final case class Start(max: Int) extends Command
  final case class Stop() extends Command
  final case class Solve(wireBox: WireBox, target: Int, requester: ActorRef[ResultFromComputer]) extends Command
  final case class ResultFromComputer(result: ComputingMessages.ComputationResult) extends Command

export Messages.*

object PekkoRoot:
  def apply(): Behavior[Command] =
    idle()

  private def idle(): Behavior[Command] =
    Behaviors.setup[Command] { context =>
      Behaviors.receiveMessage { message =>
        context.log.trace("[Idle] : Receiving message....... {}", message)

        message match
          case Start(max) =>
            context.log.debug("[Idle] : Starting {} children", max)
            val children =
              for i <- 1 to max
              yield
                context.spawn(Computer(i), s"computer_$i")

            context.log.debug("[Idle] : {} children created", children.length)
            readyToResolve(children)
          case ResultFromComputer(_) =>
            context.log.debug("[Idle] : Receiving another answer, don't take it into account")
            Behaviors.same
          case message =>
            context.log.error("[Idle] : Unmanaged message received : {}", message.getClass)
            Behaviors.unhandled
      }
    }

  private def readyToResolve(children: Seq[ActorRef[Computation]]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      context.log.trace("[Ready] : Receiving message....... {}", message.getClass)

      given ActorContext[Command] = context

      message match
        case Solve(wireBox, target, sender) =>
          val backendResponseMapper: ActorRef[ComputationResult] =
            context.messageAdapter(rsp => ResultFromComputer(rsp))
          solve(wireBox, target, children, backendResponseMapper)
          waitingFirst(children, sender, None)
        case Stop() =>
          context.log.debug("[Ready] : Stopping children")
          askChildrenToFinishAndStop(children)
          idle()
        case ResultFromComputer(value) =>
          context.log.debug("[Ready] : Receiving another answer, don't take it into account")
          Behaviors.same
        case _ => Behaviors.unhandled
    }

  private def waitingFirst(children: Seq[ActorRef[Computation]], requester: ActorRef[ResultFromComputer], result: Option[ResultFromComputer]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      context.log.trace("[WaitingFirst] : Receiving message....... {}", message.getClass)

      given ActorContext[Command] = context

      message match
        case ResultFromComputer(value) =>
          context.log.debug("[WaitingFirst] : Value is received {}", value)
          requester ! ResultFromComputer(value)
          askChildrenToFinish(children)
          readyToResolve(children)
        case Stop() =>
          context.log.debug("[WaitingFirst] : Stopping children")
          askChildrenToFinishAndStop(children)
          idle()
        case _ => Behaviors.unhandled
    }

  private def solve(wireBox: WireBox, target: Int, currentChildren: Seq[ActorRef[Computation]], sender: ActorRef[ComputationResult])(using context: ActorContext[Command]): Unit =
    context.log.debug("About to send mission with target {}", target)
    currentChildren.foreach(_ ! Mission(wireBox, target, sender))

  private def askChildrenToFinishAndStop(currentChildren: Seq[ActorRef[Computation]])(using context: ActorContext[Command]): Unit =
    askChildrenToFinish(currentChildren)
    stopChildren(currentChildren)

  private def askChildrenToFinish(currentChildren: Seq[ActorRef[Computation]])(using context: ActorContext[Command]): Unit =
    context.log.debug("Asking children to finish")
    currentChildren.foreach(current => {
      current ! Finish()
    })
  
  private def stopChildren(currentChildren: Seq[ActorRef[Computation]])(using context: ActorContext[Command]): Unit =
    context.log.debug("Stopping children")
    currentChildren.foreach(context.stop)