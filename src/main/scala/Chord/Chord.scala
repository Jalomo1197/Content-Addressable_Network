package Chord

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors, LoggerOps}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}

// TODO: Must make two acknowledgments commands. Add to Singleton and catch cases with logging

object Chord{
  // Apply function called by Chord(...)
  def apply(): Behavior[Command] =
    Behaviors.setup(context => new Chord(context))
  // trait Command to generalize the onMessage function
  trait Command
  // this Command can be processes by both Chord actor & Guardian Node actors. Passed down if possible
  case class registerNode(nodeID: Int, nodeGroupID: String) extends Command with GuardianNode.Command
  // this Command creates a new Guardian Node
  case class registerGuardianNode(nodeGroupID: String) extends Command
  // see TODO
}

class Chord(context: ActorContext[Chord.Command]) extends AbstractBehavior[Chord.Command](context) {
  // For immediate access to case classes
  import Chord._
  private var guardians = Map.empty[String, ActorRef[GuardianNode.Command]]

  override def onMessage(msg: Chord.Command): Behavior[Chord.Command] = {
    msg match {

      case registerNode(nodeID, nodeGroupID) =>
        guardians.get(nodeGroupID) match {
          case None =>
            context.log.info("Guardian Node: " + nodeGroupID + " DNE")
            context.log.info("Ignoring Request: creation of Node = " + nodeID)
            Behaviors.unhandled
          case guardianOP@Some(g) =>
            // context.log.info("") Cannot confirm here. Must confirm in acknowledgment command see TODO
            val guardian = guardianOP.get
            guardian ! registerNode(nodeID, nodeGroupID)
        }


      case registerGuardianNode(nodeGroupID) =>
        guardians.get(nodeGroupID) match {
          case None =>
            val guardian = context.spawn(GuardianNode(nodeGroupID), "Guardian Node: " + nodeGroupID + "created")
            guardians += nodeGroupID -> guardian
          case guardianOP@Some(g) =>
            context.log.info("Guardian Already Exist : " + guardianOP)
            Behaviors.unhandled
        }
    }
    this
  }



  /* Signal handling */
  override def onSignal: PartialFunction[Signal, Behavior[Command]] = {
    case PostStop =>
      context.log.info("Chord actor stopped")
      this
  }
}
