package CAN

import CAN.Procedure.{KEY_LOOKUP, KEY_STORE, NEW_NODE}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object Node{
  def apply():  Behavior[Command] = Behaviors.setup(new Node(_))

  trait Command
  // Reply from DNS, providing a bootstrap node
  case class acquiredBootstrap(p: Procedure[Bootstrap.Command]) extends Command
  // Reply from a bootstrap node, providing an exist CAN node
  case class acquiredNodeInNetwork(p: Procedure[Node.Command]) extends Command
  // Query for this node's zone, must reply
  case class getZone(p: Procedure[Node.Command]) extends Command
  // Command to set node's zone
  case class setZone(p: Procedure[Node.Command]) extends Command
  // Command to set a neighbor if valid zone
  case class setNeighbor(p: Procedure[Node.Command]) extends Command
  // For setup of the initial 4 nodes
  case class initializeNeighbors(n: List[ActorRef[Node.Command]]) extends Command
  // Beginning of routing to find zone
  case class findZone(p: Procedure[Node.Command]) extends Command
}

class Node(context: ActorContext[Node.Command]) extends AbstractBehavior[Node.Command](context){
  import Node._
  import Bootstrap.getNodeInNetwork
  import User.{queryResponse, insertConfirmed}
  // Zone for Actor is unset
  var zone: Zone = Zone((-1.0,-1.0),(-1.0,-1.0))
  var distributedMap: Map[String, String] = Map()

  override def onMessage(msg: Node.Command): Behavior[Node.Command] = {

    // Procedure to adjust the first 4 nodes' neighbors
    case initializeNeighbors(nodes) =>
      nodes.foreach(n  => n ! getZone(Procedure[Node.Command]().withReference(context.self)))
      context.log.info(s"NODE::ZONE: ${zone.formatZone} REQUESTED ZONES OF OTHER INITIAL NODES")

    // Response from DNS, procedure contains a bootstrap node
    case acquiredBootstrap(p) =>
      p.getReplyTo.get ! getNodeInNetwork(Procedure[Node.Command]().withReference(context.self))
      context.log.info(s"NODE NODE REQUESTED A NODE IN CAN FROM BOOTSTRAP")

    // Response from a bootstrap node, procedure contains a active C.A.N. node
    case acquiredNodeInNetwork(p) =>
      p.getReplyTo.get ! findZone(Procedure[Node.Command]().withReference(context.self).withZone(zone))
      context.log.info(s"NODE NODE FIND_ZONE PROCEDURE SENT TO NODE IN CAN")

    // Query for this node's zone
    case getZone(p) =>
      p.getReplyTo.get ! setNeighbor(Procedure[Node.Command]().withNeighbor(context.self).withZone(zone))
      context.log.info(s"NODE::ZONE: ${zone.formatZone} SENT ZONE INFO")

    // Command to set this node's zone
    case setZone(p) =>
      zone = p.getZone.get
      context.log.info(s"NODE::ZONE: ${zone.formatZone} ZONE SET")

    // Command to set this node's neighbor IF POSSIBLE ONLY
    case setNeighbor(p) =>
      val neighborReference = p.getNeighbor.get
      val neighborZone = p.getZone.get
      zone.set_neighbor(neighborReference, neighborZone)
      context.log.info(s"NODE::ZONE: ${zone.formatZone} SETTING NEIGHBOR::ZONE: ${neighborZone.formatZone}")

    // Procedure to utilize routing algorithm, to find point P(x,y) in space
    case findZone(p) =>
      // Extracting info for cases KEY_LOOKUP and KEY_STORE
      val location = p.getLocation.get
      val user = p.getUser.get
      // If point P is in this nodes zone
      if (zone.containsP(location)) {
        p.getRoutingPurpose.get match {  // identify purpose
          case KEY_LOOKUP =>
            // return key_value pair to user
            val key = p.getKeyLookup.get
            val value = distributedMap.get(key)
            user ! queryResponse(key, distributedMap.get(key))
            context.log.info(s"FOUND KEY: $key LOCATION: $location ZONE: ${zone.formatZone} RETURNING: ${(key, value)}")
          case KEY_STORE =>
            val (key, value) = p.getDHTpair.get
            distributedMap += (key -> value)
            user ! insertConfirmed(key, value)
            context.log.info(s"($key , $value) WITH LOCATION $location STORED IN ZONE: ${zone.formatZone} ")
          case NEW_NODE =>
            context.log.info("SPLIT NOT IMPLEMENTED")
        }
      }
      else{
        // Closest neighbor to P (that has not been visited)
        context.log.info("CLOSEST NEIGHBOR NOT IMPLEMENTED")
      }
    this
  }
}
