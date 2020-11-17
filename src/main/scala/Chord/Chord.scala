package Chord

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import net.ceedubs.ficus.Ficus._
import com.typesafe.config.{Config, ConfigFactory}



object Chord{
  // Allowing one instances of chord actor for now.
  private var ChordSingleton: Option[Chord] = None
  // Gets the Chord class
  def getChordActor: Chord = ChordSingleton.get
  // Apply function called by Chord(...)
  def apply(): Behavior[Command] =
    Behaviors.setup(context => {
      ChordSingleton = Some(new Chord(context))
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
      this Command is meant to import map values from application.config
   */
  final case class initializeNodesWithConfig(config: Config, replyTo: ActorRef[distributedMapInitialized]) extends Command
  // Used for testing createTestProbe[distributedMapInitialized]
  final case class distributedMapInitialized(dictionary: Map[String, String])
}

class Chord(context: ActorContext[Chord.Command]) extends AbstractBehavior[Chord.Command](context) {
  // For immediate access to case classes
  import Chord._
  import Node.receiveList
  //val config: Config = ConfigFactory.load()
  var dictionary: Map[String, String] = Map.empty[String, String]
  // Map for node Actors, access by NodeID
  private var nodes = Map.empty[String, ActorRef[Node.Command]]
  var kickStartNode:  String = ""



  override def onMessage(msg: Chord.Command): Behavior[Chord.Command] = {
    msg match {
      case keyLookup(key, user) =>
        /* Pass to arbitrary node to kick start CHORD algorithm
           Because Actors only process one message at a time, the
           finger table for arbitrary node should be set already  */
        nodes(kickStartNode) ! keyLookup(key, user)
      case initializeNodesWithConfig(config, replyTo) =>
        // Creating dictionary defined in config file: application.conf
        dictionary = config.as[Map[String, String]]("dictionary")
        // Calculating m bit identifier
        val m: Int = (Math.log(dictionary.size) / Math.log(2)).toInt
        // For each entry created a Node Actor and append to map
        dictionary.foreach( entry => {
          nodes += entry._1 -> context.spawn(Node(entry._1, entry._2, m), s"node-$entry._1")
          context.log.info("Node: " + entry._1 + " from dictionary added to Chord")
          if (kickStartNode.equals("")) kickStartNode = entry._1 // Saving arbitrary node
        })
        // Giving arbitrary node all other Node Actors, for Chord Algorithm operations
        nodes(kickStartNode) ! receiveList(nodes)
        // For testing. See ChordSpec.scala under test folder
        replyTo ! distributedMapInitialized(dictionary)
    }
    this
  }

  /* User obtain ActorRef to Chord Singleton via Chord.getChordActor.getReference */
  def getReference: ActorRef[Command] = context.self

  /* Signal handling */
  override def onSignal: PartialFunction[Signal, Behavior[Chord.Command]] = {
    case PostStop =>
      context.log.info("Chord actor stopped")
      this
  }
}
