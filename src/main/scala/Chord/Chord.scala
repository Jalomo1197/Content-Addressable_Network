package Chord

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}


object Chord{
  // Apply function called by Chord(...)
  def apply(nodeAmount: Int): Behavior[Command] =
    Behaviors.setup(context => new Chord(context, nodeAmount))
  // trait Command to generalize the onMessage function
  trait Command
  // command user -> insert (K,V)
  case class insertMapEntry(key :String, value: String) extends Command with Node.Command
  /*
     Chord.scala:
     this Command in this level forwards the lookup to the node that kick starts algorithm (finger table usage)
     Node.scala:
     this Command in Node.scala file, hashes the key to use finger table
  */
  case class keyLookup(key: String, user: ActorRef[User.Command]) extends Command with Node.Command
  /*
     this command is sent by a Node Actor and submitted to all other Node Actors
  */
  case class updateFingerTable(newNodeID: String, node: ActorRef[Node.Command]) extends Command with Node.Command

}

class Chord(context: ActorContext[Chord.Command], nodeAmount: Int) extends AbstractBehavior[Chord.Command](context) {
  // For immediate access to case classes
  import Chord._

  // Map for node Actors, access by NodeID
  private var nodes = Map.empty[String, ActorRef[Node.Command]]
  var IP_identification = "8.8.8.8"
  var kickStartNode:  String = ""
  var key = "google.com"
  for (n <- 0 until nodeAmount ){
    // key must be randomize too?
    // IP_identification must randomize

    // In Node.scala constructor: updateFingerTable is sent back to Chord
    val node = context.spawn(Node(IP_identification, key), s"node-$IP_identification")
    // new node now included in map

    nodes += IP_identification -> node
    // must set kick start node
    if (kickStartNode.equals("")) kickStartNode = IP_identification
    context.log.info("Node: " + IP_identification + " add to Chord")
  }


  override def onMessage(msg: Chord.Command): Behavior[Chord.Command] = {
    msg match {
      case updateFingerTable(newNodeID, node) =>
        // Sending update to all nodes
        nodes.foreach( nodeEntry => {
          context.log.info("updateFingerTable sent to Node: " + nodeEntry._1)
          nodeEntry._2 ! updateFingerTable(newNodeID, node)
        })
      case keyLookup(key, user) =>
        // pass to random node to kick start algorithm
        nodes(kickStartNode) ! keyLookup(key, user)

      case insertMapEntry(key, value) =>
        // determine which node will store based on range
        // send to node  insertMapEntry(key, value)
    }
    this
  }


  /* Signal handling */
  override def onSignal: PartialFunction[Signal, Behavior[Chord.Command]] = {
    case PostStop =>
      context.log.info("Chord actor stopped")
      this
  }
}
