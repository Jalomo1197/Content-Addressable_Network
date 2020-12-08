package CAN

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}


object DNS{
  def apply(): Behavior[Command] = Behaviors.setup(context => new DNS(context))

  trait Command
}

class DNS(context: ActorContext[DNS.Command]) extends AbstractBehavior[DNS.Command](context) {
  override def onMessage(msg: DNS.Command): Behavior[DNS.Command] = ???
}
