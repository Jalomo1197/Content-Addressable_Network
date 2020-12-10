package CAN

import akka.actor.typed.ActorRef

object Procedure extends Enumeration {
  def apply[T](): Procedure[T] = new Procedure()

  /* To identify purpose once zone is found from routing to it */
  type routing_type = Value
  val KEY_STORE, KEY_LOOKUP, NEW_NODE = Value
}

/*  A Procedure Instance can encapsulate the follow information
*   reference:          For the next send or a reply to
*   visited:            For book-keeping visited nodes
*   neighborsToUpdate:  For updating node when a split has occurred
*   location:           This is the P location (From: new_node -> random P or key -> hashed -> deterministic (X, Y))
*   keyValue:           Key value to store at location
*   keyLookup           Key to query DHT map at the node key was mapped to
*   routingPurpose:     For next actions based on the purpose of finding a zone
*   zone:               For zone assignment/querying
* */
case class Procedure[T](routingPurpose: Option[Procedure.routing_type] = None,
                        visited: Option[List[ActorRef[Node.Command]]] = None,
                        neighborsToUpdate: Option[List[Neighbor]] = None,
                        neighbor: Option[ActorRef[Node.Command]] = None,
                        key_lookup: Option[String] = None,
                        key_value: Option[(String, String)] = None,
                        location: Option[(Double, Double)] = None,
                        reference: Option[ActorRef[T]] = None,
                        zone: Option[Zone] = None,
                       ) {
  import Procedure.routing_type

  def getNeighborsToUpdate: Option[List[Neighbor]] = neighborsToUpdate
  def getRoutingPurpose: Option[routing_type] = routingPurpose
  def getNeighbor : Option[ActorRef[Node.Command]] = neighbor
  def getDHTpair: Option[(String, String)] = key_value
  def getLocation: Option[(Double, Double)] = location
  def getReplyTo: Option[ActorRef[T]] = reference
  def getZone : Option[Zone] = zone

  def withKeyToFind(s: String):  Procedure[T] =
    this.copy(key_lookup = Some(s))

  def withKeyValueToStore(pair: (String, String)): Procedure[T] =
    this.copy(key_value = Some(pair))

  def withLocation(xy: (Double, Double)) : Procedure[T] =
    this.copy(location = Some(xy))

  def withNeighborsToUpdate(n: List[Neighbor]): Procedure[T] =
    this.copy(neighborsToUpdate = Some(n))

  def withRoutingPurpose(rp: routing_type): Procedure[T] =
    this.copy(routingPurpose = Some(rp))

  def withReference(r: ActorRef[T]): Procedure[T] =
    this.copy(reference = Some(r))

  def withNeighbor(n: ActorRef[Node.Command]): Procedure[T] =
    this.copy(neighbor = Some(n))

  def withZone(z: Zone): Procedure[T] =
    this.copy(zone = Some(z))

  def withVisited(v: ActorRef[Node.Command]): Procedure[T] = {
    visited match {
      case Some(list) =>  this.copy( visited = Some(v :: list) )
      case None =>        this.copy( visited = Some(List(v)) )
    }
  }

  /*  To check if a neighbor was visited to then forward
      or NOT forward depending on return value            */
  def wasVisited(v: ActorRef[Node.Command]): Boolean = {
    visited match {
      case Some(list) =>  list.contains(v)
      case None =>        false
    }
  }
}
