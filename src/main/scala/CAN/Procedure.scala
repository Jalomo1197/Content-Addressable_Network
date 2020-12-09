package CAN

import akka.actor.typed.ActorRef

object Procedure extends Enumeration {
  def apply[T](): Procedure[T] = new Procedure()
  type routing_type = Value
  val key_store, key_lookup, new_node = Value
}

class Procedure[T] {
  import Procedure.routing_type
  private var neighbor: Option[ActorRef[Node.Command]] = None
  private var zone: Option[Zone] = None
  private var replyTo: Option[ActorRef[T]] = None
  private var routingPurpose: Option[routing_type] = None

  def withRoutingPurpose(rp: routing_type): Procedure[T] = {
    routingPurpose = Some(rp)
    this
  }

  def withReference(r: ActorRef[T]): Procedure[T] = {
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
