package Chord

import akka.actor.TypedActor.self
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps

import scala.math.BigDecimal.int2bigDecimal
import scala.math.BigInt.int2bigInt

object Node {
  def apply(groupId: String, deviceId: String, m: Int): Behavior[Command] = {
    Behaviors.setup(context => new Node(context, groupId, deviceId: String, m))
  }

  trait Command
  // Immutable Data
  case class FindPredecessor(key: String, replyTo: ActorRef[ResultRecorded]) extends Command
  final case class ResultRecorded(key: String, node: Option[Node])

  case class FindSuccessor(key: String, replyTo: ActorRef[ResultRecorded]) extends Command
  final case class FindSuccessorRecorded(key: String, node: Option[Node])

  final case class ReadKeyValuePairs(requestId: Long, replyTo: ActorRef[RespondKeyValuePairs]) extends Command
  final case class RespondKeyValuePairs(requestId: Long, value: Option[Double])

  final case class RecordKeyValuePairs(requestId: Long, value: Double, replyTo: ActorRef[KeyValuePairsRecorded]) extends Command
  final case class KeyValuePairsRecorded(requestId: Long)

  case class receiveList(actors: Map[String, ActorRef[Node.Command]]) extends Command
}

class Node(context: ActorContext[Node.Command], groupID: String, deviceId: String, m: Int)
  extends AbstractBehavior[Node.Command](context){
  import Node._

  val k: BigInt = BigInt                                           // encrypt k here
  var gotIt = false
  var nodes: Iterable[ActorRef[Node.Command]] = Iterable.empty[ActorRef[Node.Command]]
  var lastKeyValueReading: Option[Double] = None                // No Key Value Pair initially
  var predecessor: Node = this                                // Default predecessor
  val max: Int = math.pow(2, m).toInt                     // Largest m - bit value
  // Generate Hash Value for current Node
  var n: BigInt = Hash.encrypt(deviceId)
  var fingerTable: Array[FingerEntry] = new Array[FingerEntry](m)
  // Set up of finger table
  for( k <- 0 until max - 1) {
    val start = (deviceId + math.pow(2,k)).toInt % max
    val end = (start + math.pow(2,k + 1).toInt) % max
    val interval = Interval(start, end)
    fingerTable(k) = new FingerEntry(start, interval, this)
  }
  // Can just call initFingerTable but need to declare it scala wise
  // Join
  // Set Predecessor and successor Nodes
  override def onMessage(msg: Command): Behavior[Command] = {
    import Chord.keyLookup
    msg match {
      //case keyLookup =>
      case receiveList(actors: Map[String, ActorRef[Node.Command]]) =>
        gotIt = true
        nodes = actors.values
        this
      case "firstNode" =>
        context.log.debug("First Node in Hash Ring with ID: ", deviceId)
        initFingerTable()
        this
      case "newNode" =>
        // join(deviceID)
        this
      // Write
      case FindPredecessor(key, replyTo) =>
        val k = Hash.encrypt(key)           // What we are looking for
        val start = fingerTable(0).getStart
        val end = this.successor.fingerTable(0).getInterval.get_end
        val interval = Interval(start, end)
        if (interval.valid(k)){                                         // Reachable
          val closest = closest_preceding_finger(n)               // Look for predecessor counter clockwise
          if(closest.deviceId == deviceId)                              // ONE Node
            replyTo ! ResultRecorded(deviceId, Some(this))
          else
            FindPredecessor(key, replyTo)
        }
        else
          replyTo ! ResultRecorded(deviceId, Some(this))
        this
       // Key Lookup
      case FindSuccessor(key, replyTo) =>
        // Check within range
        val start = fingerTable(0).getStart
        val end = this.successor.fingerTable(0).getInterval.get_end
        val interval = Interval(start, end)
        if (interval.valid(k)) {                                    // Reachable
          val closest = closest_preceding_finger(k)
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
      context.log.info2("Node actor {}-{} stopped", groupID, deviceId)
      this
  }
  def ithFinger_start(i: Int): BigInt = {
    val distance = Math.pow(2, i).toInt
    (n + BigInt(distance)) % max
  }
  def initFingerTable(): Unit = {
    for(i <- 0 until m){
      val start = ithFinger_start(i)
      fingerTable(i) = new FingerEntry(start, Interval(start, ithFinger_start(i + 1)), self)
    }
    predecessor = this          // Set predecessor to self
  }
  def closest_preceding_finger(id: BigInt): Node = {
    // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent, right?
    for (i <- m to 1) {                   // Scan what we are looking for from farthest to closest
      val start = ithFinger_start(i)
      val end = id
      val key = fingerTable(i).node.deviceId
      val k = Hash.encrypt(key)
      val interval = Interval(start, end)
      if (interval.valid(k))
        return fingerTable(i).node            // Found node closest to what I'm looking for
    }
    this                                      // current node is closest lol all that scanning for nothing (get fucked kid)
  }
  // By definition the first entry is the successor
  def successor: Node =
    fingerTable(0).node
}
