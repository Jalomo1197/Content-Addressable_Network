package CAN

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object Node{
  def apply(X: Double, Y: Double):  Behavior[Command] = Behaviors.setup(context => new Node(context, X, Y))

  trait Command
  case class acquireBootstrap(p: Procedure[Bootstrap.Command]) extends Command
}

class Node(context: ActorContext[Node.Command], X: Double, Y:Double) extends AbstractBehavior[Node.Command](context){
  import Node.acquireBootstrap

  override def onMessage(msg: Node.Command): Behavior[Node.Command] = {
    case acquireBootstrap(p) => ???

    this
  }
}
