package CAN

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object Node{
  def apply():  Behavior[Command] = Behaviors.setup(context => new Node(context))

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
  // Zone for Actor is unset
  var zone: Zone = Zone((-1.0,-1.0),(-1.0,-1.0))

  override def onMessage(msg: Node.Command): Behavior[Node.Command] = {
    case acquiredBootstrap(p) =>
      p.getReplyTo.get ! getNodeInNetwork(Procedure[Node.Command]().withReference(context.self))

    case acquiredNodeInNetwork(p) =>
      p.getReplyTo.get ! findZone(Procedure[Node.Command]().withReference(context.self).withZone(zone))

    case setZone(p) =>
      zone = p.getZone.get

    case initializeNeighbors(nodes) =>
      nodes.foreach(n  => n ! getZone(Procedure[Node.Command]().withReference(context.self)))

    case getZone(p) =>
      p.getReplyTo.get ! setNeighbor(Procedure[Node.Command]().withNeighbor(context.self).withZone(zone))

    case setNeighbor(p) =>
      zone.set_neighbor(p.getNeighbor.get, p.getZone.get)

    case findZone(p) => ???

    this
  }
}
