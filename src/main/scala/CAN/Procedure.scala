package CAN

import akka.actor.typed.ActorRef

object Procedure{
  def apply[T](): Procedure[T] = new Procedure()
}

class Procedure[T] {
  private var neighbor: Option[ActorRef[Node.Command]] = None
  private var zone: Option[Zone] = None
  private var replyTo: Option[ActorRef[T]] = None

  def withReply(r: ActorRef[T]): Procedure[T] = {
    replyTo = Some(r)
    this
  }

  def withNeighbor(n: ActorRef[Node.Command]): Procedure[T] = {
    neighbor = Some(n)
    this
  }

  def withZone(z: Zone): Procedure[T] = {
    zone = Some(z)
    this
  }

  def getNeighbor : Option[ActorRef[Node.Command]] = neighbor
  def getZone : Option[Zone] = zone
  def getReplyTo: Option[ActorRef[T]] = replyTo
}
