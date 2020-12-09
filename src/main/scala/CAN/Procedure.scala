package CAN

import akka.actor.typed.ActorRef

object Procedure extends Enumeration {
  def apply[T](): Procedure[T] = new Procedure()
  type routing_type = Value
  val KEY_STORE, KEY_LOOKUP, NEW_NODE = Value

  sealed trait ProcedureType
  object ProcedureType{
    sealed trait HasReference extends ProcedureType
    sealed trait HasZone extends ProcedureType
    sealed trait HasNeighbor extends ProcedureType
    sealed trait RoutingType extends ProcedureType

    type Destination = HasReference
  }
}

class Procedure[T] {
  import Procedure.routing_type
  private var neighbor: Option[ActorRef[Node.Command]] = None
  private var zone: Option[Zone] = None
  private var replyTo: Option[ActorRef[T]] = None
  private var routingPurpose: Option[routing_type] = None

  def getNeighbor : Option[ActorRef[Node.Command]] = neighbor
  def getZone : Option[Zone] = zone
  def getReplyTo: Option[ActorRef[T]] = replyTo

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
}
