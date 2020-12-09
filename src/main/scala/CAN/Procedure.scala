package CAN

import akka.actor.typed.ActorRef

object Procedure extends Enumeration {
  def apply[T](): Procedure[T] = new Procedure()

  /* To identify purpose once zone is found from routing to it */
  type routing_type = Value
  val KEY_STORE, KEY_LOOKUP, NEW_NODE = Value
}

case class Procedure[T](reference: Option[ActorRef[T]] = None,
                        visited: Option[List[ActorRef[Node.Command]]],
                        routingPurpose: Option[Procedure.routing_type] = None,
                        zone: Option[Zone] = None, neighbor: Option[ActorRef[Node.Command]] = None
                       ) {
  import Procedure.routing_type


  def getNeighbor : Option[ActorRef[Node.Command]] = neighbor
  def getZone : Option[Zone] = zone
  def getReplyTo: Option[ActorRef[T]] = reference

  def withRoutingPurpose(rp: routing_type): Procedure[T] =
    this.copy(routingPurpose = Some(rp))

  def withReference(r: ActorRef[T]): Procedure[T] =
    this.copy(reference = Some(r))

  def withNeighbor(n: ActorRef[Node.Command]): Procedure[T] =
    this.copy(neighbor = Some(n))

  def withZone(z: Zone): Procedure[T] =
    this.copy(zone = Some(z))

  def withVisited(v: ActorRef[Node.Command]): Procedure[T] ={
    visited match {
      case Some(list) =>  this.copy( visited = Some(v :: visited.get) )
      case None =>        this.copy( visited = Some(List(v)) )
    }
  }

}
