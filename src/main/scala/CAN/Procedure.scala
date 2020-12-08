package CAN

import akka.actor.typed.ActorRef

object Procedure{
  def apply[T](R: ActorRef[T]): Procedure[T] = new Procedure(R)
}

class Procedure[T](replyTo: ActorRef[T]) {
  def getReplyTo: ActorRef[T] = replyTo
}
