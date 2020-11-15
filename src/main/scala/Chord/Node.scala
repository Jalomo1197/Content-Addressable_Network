package Chord

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt


object Node {
  def apply(groupId: String, deviceId: Int, m: Int, node_count: Int): Behavior[Command] =
    Behaviors.setup(context => new Node(context, groupId, deviceId, m, node_count))

  trait Command
  // Immutable Data
  final case class ReadKeyValuePairs(requestId: Long, replyTo: ActorRef[RespondKeyValuePairs]) extends Command
  final case class RespondKeyValuePairs(requestId: Long, value: Option[Double])
  final case class RecordKeyValuePairs(requestId: Long, value: Double, replyTo: ActorRef[KeyValuePairsRecorded]) extends Command
  final case class KeyValuePairsRecorded(requestId: Long)
  case class FindSuccessor(node_id: Int) extends Command
}

class Node(context: ActorContext[Node.Command], groupId: String, deviceId: Int, m: Int, node_count: Int)
  extends AbstractBehavior[Node.Command](context){
  import Node._

  var lastKeyValueReading: Option[Double] = None        // No Key Value Pair initially
  var predecessor: Int = 0                         // Default predecessor
  val max: Int = math.pow(2, m).toInt                   // Largest m - bit value
  // Generate Hash Value for current Node
  var hash: String = Hash.getHash(deviceId.toString)
  var fingerTable: Array[Array[Int]] = Array.ofDim[Int](m, 2)
  context.log.info2("Node actor {}-{} started", groupId, deviceId)
  // Set up of finger table
  for( k <- 0 until max - 1) {
    val start = (deviceId + math.pow(2,k)).toInt % max
    val end = (start + math.pow(2,k + 1).toInt) % max
    val interval = withinInterval(start, end, m)
    //fingerTable(k) = (deviceId + math.pow(2, k).toInt) % node_count   // TODO:
  }
  // Set Predecessor and successor Nodes
  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
      case "firstNode" =>
        context.log.debug("First Node in Hash Ring with ID: ", deviceId)
        initFingerTable()
        this
      case "newNode" =>
        // join(deviceID)
        this
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
  def ithFinger_start(i: Int): Int = {
    (deviceId + Math.pow(2, i).toInt) % Math.pow(2, m).toInt
  }
  def initFingerTable(): Unit = {
    for(i <- 0 until m){
        fingerTable(i)(0) = ithFinger_start(i)
        fingerTable(i)(1) = deviceId
    }
    predecessor = deviceId  // Set predecessor to self
  }
  def FindSuccessor(id: Int): Int = {
    val predecessor = FindPredecessor(id)
    if(deviceId != predecessor){
      // Actor reference
        fingerTable(0)(1)
    }
    else
      fingerTable(0)(1) // Single Node
  }
  def FindPredecessor(id: Int): Int = {
    val potential_successor = fingerTable(0)(1)
    var closest = deviceId
    if(deviceId != potential_successor){                              // Ensure not just ONE Node
      while (withinInterval(deviceId, potential_successor, id)){      // Backtrack within log(n) range
        closest = closest_preceding_finger(id)
      }
      closest
    }
    else
      deviceId                                                        // Single Node
  }
  def closest_preceding_finger(id: Int): Int = {
    for(i <- m - 1 to 0){                 // Check Finger Table from bottom to top
        if(withinInterval(deviceId, id, fingerTable(i)(1)))
          return fingerTable(i)(1)       // node closest to id
    }
    deviceId                            // current node is closest
  }
  def exist(id: Int): Boolean = {
    if(id == deviceId) return true
    var exist = false
    /*
    TODO:
      How can we use GuardianNode to find if Node with id exist
     */
    exist
  }
}
