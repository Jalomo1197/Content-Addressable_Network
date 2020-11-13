package Chord

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}




object GuardianNode{
  def apply(nodeGroupId: String):  Behavior[Command] =
    Behaviors.setup(context => new GuardianNode(context, nodeGroupId))

    trait Command
    // this Command passes down the KEY lookup to appropriate Node (determination is up to you) if exist
    case class forward_lookup(key: Int, nodeID: Int, user: ActorRef[User.Command]) extends Command with Node.Command
    /* External Commands:
          Chord.scala : registerNode(nodeID: Int, nodeGroupID: String)
    */
}

class GuardianNode(context: ActorContext[GuardianNode.Command], nodeGroupId: String) extends AbstractBehavior[GuardianNode.Command](context) {
  // For immediate access to commands
  import GuardianNode._
  // For access to external commands
  import Chord.registerNode
  // Map for nodes
  private var nodeList = Map.empty[Int, ActorRef[Node.Command]]
  // JACOB VARIABLE FOR ALGO
  var node_count: Int = 0
  // JACOB VARIABLE FOR ALGO
  var query_count: Int = 0

  override def onMessage(msg: GuardianNode.Command):  Behavior[GuardianNode.Command] =
    msg match {
      // forwarding the KEY lookup to node responsible for inclusive range (ALGO STUFF)
      case forward_lookup(key, node, user) =>
        nodeList.get(node) match {
          case None =>
            Behaviors.unhandled
          case Some(nodeActor) =>
            nodeActor ! forward_lookup(key, node, user)
        }
        this

      // Registering node
      case newNode @ registerNode(nodeID, nodeGroupID) =>
        nodeList.get(nodeID) match {
          case None =>
            //groupId: String, deviceId: Int, m: Int, node_count: Int
            val Node = context.spawn(Node(nodeGroupID, nodeID, 0), s"node-$nodeID")
            nodeList += nodeID -> Node
        }
        this
    }
}
