package Chord

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed._
import scala.Predef.ArrowAssoc



object GuardianNode{
  def apply(nodeGroupId: String):  Behavior[Command] =
    Behaviors.setup(context => new GuardianNode(context, nodeGroupId))

    trait Command
    // query to process
    case class forward_lookup(key: Int, nodeID: Int, user: ActorRef[User.Command]) extends Command with Node.Command
    //case class registerNode(nodeID: Int, nodeGroupID: String) extends Command
}

class GuardianNode(context: ActorContext[GuardianNode.Command], nodeGroupId: String) extends AbstractBehavior[GuardianNode.Command](context) {
  import GuardianNode._
  import Chord.registerNode
  import Node._
  private var nodeList = Map.empty[Int, ActorRef[Node.Command]]
  var node_count: Int = 0
  var query_count: Int = 0

  override def onMessage(msg: GuardianNode.Command):  Behavior[GuardianNode.Command] =
    msg match {
      case forward_lookup(key, node, user) =>
        nodeList.get(node) match {
          case None =>
            Behaviors.unhandled
          case nodeToAdd @ Some(nodeActor) =>
            val toForwardNode = nodeToAdd.get
            toForwardNode ! forward_lookup(key, node, user)
        }
        this


      case newNode @ registerNode(nodeID, nodeGroupID) =>
        nodeList.get(nodeID) match {
          case None =>
            //groupId: String, deviceId: Int, m: Int, node_count: Int
            val Node = context.spawn(Node(nodeGroupID, nodeID, 0), s"node-$nodeID")
            nodeList += nodeID -> Node

        }
        this
    }
        /*

      case newNode @ registerNode(nodeID, nodeGroupID) =>
        nodeList.get(nodeID) match {
          case None =>
            //groupId: String, deviceId: Int, m: Int, node_count: Int
            val Node = context.spawn(Node(nodeGroupID, nodeID, 0), s"node-$nodeID")
            nodeList += nodeID -> Node

          case node @ Some(nodeActor) =>
            val toForwardNode = node.get
            toForwardNode ! forward_lookup(key, node, user)
        }
      }
      this
*/
}
