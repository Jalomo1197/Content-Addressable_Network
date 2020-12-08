package CAN

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object Bootstrap{
  def apply():  Behavior[Command] = Behaviors.setup(context => new Bootstrap(context))

  trait Command
}

class Bootstrap(context: ActorContext[Bootstrap.Command]) extends AbstractBehavior[Bootstrap.Command](context){
  override def onMessage(msg: Bootstrap.Command): Behavior[Bootstrap.Command] = ???
}

