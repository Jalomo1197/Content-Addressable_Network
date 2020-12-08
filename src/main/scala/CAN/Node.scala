package CAN

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object Node{
  def apply():  Behavior[Command] = Behaviors.setup(context => new Node(context))

  trait Command
  case class acquiredBootstrap(p: Procedure[Bootstrap.Command]) extends Command
  case class acquiredNodeInNetwork(p: Procedure[Node.Command]) extends Command
  case class getZone(p: Procedure[Node.Command]) extends Command
  case class setZone(z: Zone) extends Command
  case class setNeighbor(p: Procedure[Node.Command]) extends Command
  case class initializeNeighbors(n: List[ActorRef[Node.Command]]) extends Command
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
      p.getReplyTo.get ! ???

    case setZone(z) =>
      zone = z

    case initializeNeighbors(nodes) =>
      nodes.foreach(n  => n ! getZone(Procedure[Node.Command]().withReference(context.self)))

    case getZone(p) =>
      p.getReplyTo.get ! setNeighbor(Procedure[Node.Command]().withNeighbor(context.self).withZone(zone))

    case setNeighbor(p) =>
      zone.set_neighbor(p.getNeighbor.get, p.getZone.get)

    this
  }
}
