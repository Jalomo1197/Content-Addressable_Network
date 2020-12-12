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
  /** Command to join the new node to network. New node only has to shoot updates to its neighbors */
  case class joinNetwork(p: Procedure[Node.Command]) extends Command
}

class Node(context: ActorContext[Node.Command]) extends AbstractBehavior[Node.Command](context){
  import Node._
  import Bootstrap.getNodeInNetwork
  import User.{queryResponse, insertConfirmed}
  // Zone for Actor is unset
  var nodeSpawns = 0
  var zone: Zone = Zone((-1.0,-1.0),(-1.0,-1.0))
  var distributedMap: Map[String, String] = Map()
  context.log.info(s"NODE CREATED: ${context.self.path}")

  override def onMessage(msg: Node.Command): Behavior[Node.Command] = {
    msg match {
      // Procedure to adjust the first 4 nodes' neighbors
      case initializeNeighbors(nodes) =>
        nodes.foreach(n  => n ! getZone(Procedure[Node.Command]().withReference(context.self)))
        context.log.info(s"NODE::ZONE: ${zone.formatZone} REQUESTED ZONES OF OTHER INITIAL NODES")

      // Response from DNS, procedure contains a bootstrap node
      case acquiredBootstrap(p) =>
        p.getReference.get ! getNodeInNetwork(Procedure[Node.Command]().withReference(context.self))
        context.log.info(s"NODE NODE REQUESTED A NODE IN CAN FROM BOOTSTRAP")

      // Response from a bootstrap node, procedure contains a active C.A.N. node
      case acquiredNodeInNetwork(p) =>
        p.getReference.get ! findZone(Procedure[Node.Command]().withReference(context.self).withZone(zone))
        context.log.info(s"NODE NODE FIND_ZONE PROCEDURE SENT TO NODE IN CAN")

      // Query for this node's zone
      case getZone(p) =>
        p.getReference.get ! setNeighbor(Procedure[Node.Command]().withNeighbor(context.self).withZone(zone))
        context.log.info(s"NODE::ZONE: ${zone.formatZone} SENT ZONE INFO")

      // Command to set this node's zone
      case setZone(p) =>
        zone = p.getZone.get
        zone.setReference(context.self)
        context.log.info(s"NODE::ZONE: ${zone.formatZone} ZONE SET")

      // Command to set this node's neighbor IF POSSIBLE ONLY
      case setNeighbor(p) =>
        val neighborReference = p.getNeighbor.get
        val neighborZone = p.getZone.get
        zone.set_neighbor(neighborReference, neighborZone)
        context.log.info(s"NODE::ZONE: ${zone.formatZone} SETTING NEIGHBOR::ZONE: ${neighborZone.formatZone}")

      case joinNetwork(p) =>
        // Obtain data pertaining to this zone
        distributedMap = p.getKeyValueTransfers.get
        // Update zone with new assigned zone
        zone = p.getZone.get
        // Send updates to neighbors
        sendUpdatesToNeighbors()
        context.log.info("NEW NODE SENT UPDATE TO NEIGHBORS")


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
              /*if (distributedMap.size > 25) {
                context.self ! findZone(Procedure[Node.Command]().withReference(context.spawn(Node(), s"node-${context.self.path}-$nodeSpawns")).withLocation(zone.get_XRange._1,zone.get_YRange._1))
                nodeSpawns += 1
              }*/

            case NEW_NODE =>
              context.log.info(s"NODE:ZONE:: ${zone.formatZone} SPLIT PROCEDURE")
              val newNodeRef = p.getReference.get
              // New Zone for node node. Which contains the same neighbors as this zone
              val newZone = zone._splitZone
              // This node's zone updating its neighborTable to include the new node
              zone.set_neighbor(newNodeRef, newZone)
              // New node's zone updating its neighborTable to include this node
              newZone.set_neighbor(context.self, zone)
              // Now both zone have appropriate zones and neighborTables
              // Next (Key, Value) transfers, modify Procedure
              var joinProcedure = Procedure[Node.Command]().withZone(newZone)
              var keysInNewZone: List[String] = List()
              distributedMap.keys.foreach(key => {
                val locationOfKey = Zone.findLocation(key)
                if(!zone.containsP(locationOfKey))
                  keysInNewZone +:= key
              })
              // Transferring (key, value) through Procedure and removing from this map
              keysInNewZone.foreach( key => {
                joinProcedure = joinProcedure.withKeyValueTransfer(key, distributedMap(key))
                distributedMap = distributedMap - key
              })
              // Sending join C.A.N command to the newNode
              newNodeRef ! joinNetwork(Procedure[Node.Command]().withZone(newZone))
              sendUpdatesToNeighbors()
              context.log.info("MODIFIED NODE SENT UPDATE TO NEIGHBORS")
              //newNodeRef ! split(Procedure[Node.Command]().withReference(context.self))
              context.log.info(s"MODIFIED:NODE::ZONE: ${zone.formatZone} NEW:NODE::ZONE: ${newZone.formatZone} SPLIT ACCOMPLISHED")
              context.log.info(s"KEYS TRANSFERRED: $keysInNewZone FROM ZONE: ${zone.formatZone} -> TO ZONE: ${newZone.formatZone}")
          }
        }
        else{
          // Closest neighbors to P (that has not been visited)
          val closetNeighborsToLocation = zone.closestPointToP(p)
          if (closetNeighborsToLocation.nonEmpty) {
            closetNeighborsToLocation.head ! findZone(p.withVisited(context.self))
            context.log.info(s"NODE::ZONE: ${zone.formatZone} DOES NOT CONTAIN LOCATION: $location. FORWARDING PROCEDURE TO OPTIMAL NEIGHBOR")
          }
          else
            context.log.warn(s"ROUTING TO P:$location FAIL. NO OPTIMAL PROCEDURE FORWARDING")
        }
    }
    this
  }

  def sendUpdatesToNeighbors(): Unit ={
    zone.neighborTable.neighbors.foreach(neighbor => {
      neighbor.getNode match {
        case null =>
        case ref =>
          ref ! setNeighbor(Procedure[Node.Command]()
            .withReference(context.self)
            .withZone(zone))
      }
    })
  }
}
