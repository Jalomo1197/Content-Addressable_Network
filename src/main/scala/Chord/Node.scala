package Chord

import akka.actor.TypedActor.self
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps

object Node {
  def apply(groupId: String, deviceId: Int, m: Int, node_count: Int): Behavior[Command] =
    Behaviors.setup(context => new Node(context, groupId, deviceId, m, node_count))

  trait Command
  // Immutable Data
  case class FindPredecessor(key: Int, replyTo: ActorRef[ResultRecorded]) extends Command
  final case class ResultRecorded(key: Int, node: Option[Node])

  case class FindSuccessor(key: Int, replyTo: ActorRef[ResultRecorded]) extends Command
  final case class FindSuccessorRecorded(key: Int, node: Option[Node])

  final case class ReadKeyValuePairs(requestId: Long, replyTo: ActorRef[RespondKeyValuePairs]) extends Command
  final case class RespondKeyValuePairs(requestId: Long, value: Option[Double])

  final case class RecordKeyValuePairs(requestId: Long, value: Double, replyTo: ActorRef[KeyValuePairsRecorded]) extends Command
  final case class KeyValuePairsRecorded(requestId: Long)

  case class receiveList(actors: List[ActorRef[Node.Command]]) extends Command
}

class Node(context: ActorContext[Node.Command], groupId: String, deviceId: Int, m: Int, node_count: Int)
  extends AbstractBehavior[Node.Command](context){
  import Node._

  var lastKeyValueReading: Option[Double] = None        // No Key Value Pair initially
  var predecessor: Node = this                         // Default predecessor
  val max: Int = math.pow(2, m).toInt                   // Largest m - bit value
  // Generate Hash Value for current Node
  var hash: String = Hash.getHash(deviceId.toString)
  var fingerTable: Array[FingerEntry] = new Array[FingerEntry](m)
  context.log.info2("Node actor {}-{} started", groupId, deviceId)
  // Set up of finger table
  for( k <- 0 until max - 1) {
    val start = (deviceId + math.pow(2,k)).toInt % max
    val end = (start + math.pow(2,k + 1).toInt) % max
    val interval = Interval(start, end)
    fingerTable(k) = new FingerEntry(start, interval, this)
  }
  // Join

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
      case FindPredecessor(key, replyTo) =>
        // Check within range
        val start = deviceId + 1 % max
        val end = (this.successor.deviceId + 1) % max
        val interval = Interval(start, end)
        if (interval.valid(key)){                                         // Reachable
          val closest = closest_preceding_finger(deviceId)               // Look for predecessor counter clockwise
          if(closest.deviceId == deviceId)                              // ONE Node
            replyTo ! ResultRecorded(deviceId, Some(this))
          else
            FindPredecessor(key, replyTo)
        }
        else
          replyTo ! ResultRecorded(deviceId, Some(this))
        this
      case FindSuccessor(key, replyTo) =>
        // Check within range
        val start = deviceId + 1 % max
        val end = (this.successor.deviceId + 1) % max
        val interval = Interval(start, end)
        if (interval.valid(key)) {                                    // Reachable
          val closest = closest_preceding_finger(deviceId)
          if(closest.deviceId == deviceId)                                // ONE Node
            replyTo ! ResultRecorded(deviceId, Some(this.successor))
          else
            FindSuccessor(key, replyTo)
        }
        else
          replyTo ! ResultRecorded(deviceId, Some(this.successor))
        this
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
  def ithFinger_start(i: Int): Int = {
    (deviceId + Math.pow(2, i).toInt) % max
  }
  def initFingerTable(): Unit = {
    for(i <- 0 until m){
      val start = ithFinger_start(i)
      fingerTable(i) = new FingerEntry(start, Interval(start, ithFinger_start(i + 1)), self)
    }
    predecessor = this          // Set predecessor to self
  }
  def closest_preceding_finger(id: Int): Node = {
    // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent, right?
    for (i <- m - 1 to 0) {                   // Scan what we are looking for from farthest to closest
      val start = ithFinger_start(i)
      val end = id
      val key = fingerTable(i).node.deviceId
      val interval = Interval(start, end)
      if (interval.valid(key))
        return fingerTable(i).node            // Found node closest to what I'm looking for
    }
    this                                      // current node is closest lol all that scanning for nothing (get fucked kid)
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
  // By definition the first entry is the successor
  def successor: Node =
    fingerTable(0).node
}
