package Chord

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps


object Node {
  def apply(groupId: String, deviceId: Int, m: Int, node_count: Int): Behavior[Command] =
    Behaviors.setup(context => new Node(context, groupId, deviceId, m, node_count))

  sealed trait Command
  // Immutable Data
  final case class ReadKeyValuePairs(requestId: Long, replyTo: ActorRef[RespondKeyValuePairs]) extends Command
  final case class RespondKeyValuePairs(requestId: Long, value: Option[Double])
  final case class RecordKeyValuePairs(requestId: Long, value: Double, replyTo: ActorRef[KeyValuePairsRecorded]) extends Command
  final case class KeyValuePairsRecorded(requestId: Long)
}

class Node(context: ActorContext[Node.Command], groupId: String, deviceId: Int, m: Int, node_count: Int)
  extends AbstractBehavior[Node.Command](context) {
  import Node._

  var lastKeyValueReading: Option[Double] = None        // No Key Value Pair initially
  var predecessor: Node = this                          // Default predecessor
  val max: Int = math.pow(2, m).toInt                   // Largest m - bit value
  // Generate Hash Value for current Node
  var hash: String = Hash.getHash(deviceId.toString)
  var fingerTable = new Array[Int](max)
  context.log.info2("Node actor {}-{} started", groupId, deviceId)
  // Set up of finger table
  for( k <- 0 until max - 1) {
    val start = (deviceId + math.pow(2,k)).toInt % max
    val end = (start + math.pow(2,k + 1).toInt) % max
    val interval = withinInterval(start, end, m)
    fingerTable(k) = (deviceId + math.pow(2, k).toInt) % node_count   // TODO:
  }
  // Set Predecessor and successor Nodes

  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
      // Write
      case RecordKeyValuePairs(id, value, replyTo) =>
        context.log.info2("Node Hash reading {} with {}", value, id)
        lastKeyValueReading = Some(value)
        replyTo ! KeyValuePairsRecorded(id)
        this
      // Read
      case ReadKeyValuePairs(id, replyTo) =>
        replyTo ! RespondKeyValuePairs(id, lastKeyValueReading)         // Send Key Value pair
        this
    }
  }
  override def onSignal: PartialFunction[Signal, Behavior[Command]] = {
    case PostStop =>
      context.log.info2("Node actor {}-{} stopped", groupId, deviceId)
      this
  }
  // (start, end]
  def withinInterval(predecessor: Int, successor: Int, index: Int): Boolean = {
    var distance = 1
    if(predecessor == index)  return true
    // Successor not yet reached || distance not reached max
    while(((distance + predecessor) % max != successor) && distance < max){
      if(((predecessor + distance) % max) == index) return true // Reachable
      else
        distance += 1
    }
    false // Unreachable
  }
}
