package Chord

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}


object Chord{
  // Allowing one instances of chord actor for now.
  private var ChordSingleton: Option[Chord] = None

  def getChordActor: Chord = ChordSingleton.get

  // Apply function called by Chord(...)
  def apply(nodeAmount: Int): Behavior[Command] =
    Behaviors.setup(context => {
      ChordSingleton = Some(new Chord(context, nodeAmount))
      ChordSingleton.get
    })




  // trait Command to generalize the onMessage function
  trait Command
  /*
     Chord.scala:
     this Command in this level forwards the lookup to the node that kick starts algorithm (finger table usage)
     Node.scala:
     this Command in Node.scala file, hashes the key to use finger table
  */
  final case class keyLookup(key: String, user: ActorRef[User.Command]) extends Command with Node.Command
  /*
      this Command is meant to be sent back to a User Actor by Node Actor
      Both Node.scala (to send) & User.scala (to receive) must import
   */
  //final case class RespondKeyValuePairs(requestId: Long, value: Option[Double])
}

class Chord(context: ActorContext[Chord.Command], nodeAmount: Int) extends AbstractBehavior[Chord.Command](context) {
  // For immediate access to case classes
  import Chord._
  import Node.receiveList

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

  nodes(kickStartNode) ! receiveList(nodes)

  override def onMessage(msg: Chord.Command): Behavior[Chord.Command] = {
    import Node.{FindSuccessor}
    msg match {
      case keyLookup(key, user) =>
        // Pass to random node to kick start algorithm
        // Because Actors only process one message at a time, the finger for kickStartNode should be set
        nodes(kickStartNode) ! FindSuccessor(key, user)
    }
    this
  }


  def getReference: ActorRef[Command] = context.self

  /* Signal handling */
  override def onSignal: PartialFunction[Signal, Behavior[Chord.Command]] = {
    case PostStop =>
      context.log.info("Chord actor stopped")
      this
  }
}
