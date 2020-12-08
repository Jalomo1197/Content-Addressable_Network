package CAN

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object Node{
  def apply():  Behavior[Command] = Behaviors.setup(context => new Node(context))

  trait Command
}

class Node(context: ActorContext[Node.Command]) extends AbstractBehavior[Node.Command](context){
  override def onMessage(msg: Node.Command): Behavior[Node.Command] = ???
}
