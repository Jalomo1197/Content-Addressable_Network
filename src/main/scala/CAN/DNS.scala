package CAN

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}


object DNS{
  def apply(): Behavior[Command] = Behaviors.setup(context => new DNS(context))

  trait Command
  case class bootstrap(p: Procedure) extends Command
}

class DNS(context: ActorContext[DNS.Command]) extends AbstractBehavior[DNS.Command](context) {
  import DNS.bootstrap
  var boots = 1

  var bootstraps: List[ActorRef[Bootstrap.Command]] = List(context.spawn(Bootstrap(), s"node-bootstrap-$boots"))

  override def onMessage(msg: DNS.Command): Behavior[DNS.Command] = {
    case bootstrap(procedure) =>



    this
  }
}
