package CAN

import akka.actor.typed.ActorRef

object Procedure{
  def apply(R: ActorRef[Node.Command]): Procedure = new Procedure(R)
}

class Procedure(replyTo: ActorRef[Node.Command]) {

}
