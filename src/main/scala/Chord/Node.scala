package Chord

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import scala.math.BigInt.int2bigInt
/*
*  (key, value)
* */
object Node {
  def apply(key: String, value: String): Behavior[Command] = {
    Behaviors.setup(context => new Node(context, key: String, value: String))
  }
  trait Command
  // Immutable Data
  case class FindPredecessor(key: String, replyTo: ActorRef[ResultRecorded]) extends Command
  final case class ResultRecorded(key: String, node: Option[Node])

  case class FindSuccessor(key: String, replyTo: ActorRef[User.Command]) extends Command
  final case class FindSuccessorRecorded(key: String, node: Option[Node])

  final case class ReadKeyValuePairs(key: String, replyTo: ActorRef[RespondKeyValuePairs]) extends Command
  final case class RespondKeyValuePairs(key: String, value: Option[String], replyTo: ActorRef[User.Command])

  final case class RecordKeyValuePairs(key: String, value: Option[String], replyTo: ActorRef[KeyValuePairsRecorded]) extends Command
  final case class KeyValuePairsRecorded(key: String, value: Option[String])

  case class receiveList(actors: Map[String, ActorRef[Node.Command]]) extends Command
}

class Node(context: ActorContext[Node.Command], key: String, value: String)
  extends AbstractBehavior[Node.Command](context){
  import Node._
  import Chord.keyLookup
  import User.queryResponse
  var m: Int = 0
  var k: BigInt = 0                                           // encrypt k here
  var gotIt = false
  var nodes: Iterable[ActorRef[Node.Command]] = Iterable.empty[ActorRef[Node.Command]]
  var lastKeyValueReading: Map[String, Option[String]] = Map.empty

  // Default predecessor
  var predecessor: Node = this
  // Largest m - bit value
  val max: Int = math.pow(2, m).toInt
  // Generate Hash Value for current Node
  var n: BigInt = Hash.encrypt(key)
  var fingerTable: Array[FingerEntry] = new Array[FingerEntry](m)
  // Set up of finger table
  initFingerTable()
  // Can just call initFingerTable but need to declare it scala wise
  // Join
  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
      case keyLookup(key, user) =>
        // Check within range
        val start = fingerTable(0).getStart
        val end = this.successor.fingerTable(0).getInterval.get_end
        val interval = Interval(start, end)
        val hash = Hash.encrypt(key)
        if (interval.valid(hash)) {                                    // Reachable
          val closest = closest_preceding_finger(k)
          val value = lastKeyValueReading.getOrElse(this.successor.key, None)
          if(closest.key == key)                                // ONE Node
            user ! queryResponse(key, value)
          else
            //FindSuccessor(key, replyTo)
        }
        else
          //replyTo ! ResultRecorded(key, Some(this.successor))
        this
      //case queryResponse(key, value) =>
      //this
      case receiveList(actors: Map[String, ActorRef[Node.Command]]) =>
        gotIt = true
        nodes = actors.values
        m = nodes.size
        this
      // Write
      case FindPredecessor(key, replyTo) =>
        if(key == this.key){
          lastKeyValueReading += key -> this.n
        }
        val k = Hash.encrypt(key)           // What we are looking for
        val start = fingerTable(0).getStart
        val end = this.successor.fingerTable(0).getInterval.get_end
        val interval = Interval(start, end)
        if (interval.valid(k)){                                         // Reachable
          val closest = closest_preceding_finger(n)                   // Look for predecessor counter clockwise
          if(closest.key == key)                              // ONE Node
            replyTo ! ResultRecorded(key, Some(this))
          else
            FindPredecessor(key, replyTo)
        }
        else
          replyTo ! ResultRecorded(key, Some(this))
        this
       // Key Lookup
      case FindSuccessor(key, replyTo) =>
        // Check within range
        val start = fingerTable(0).getStart
        val end = this.successor.fingerTable(0).getInterval.get_end
        val interval = Interval(start, end)
        if (interval.valid(k)) {                                    // Reachable
          val closest = closest_preceding_finger(k)
          if(closest.key == key)                                // ONE Node
            replyTo ! ResultRecorded(key, Some(this.successor))
          else
            FindSuccessor(key, replyTo)
        }
        else
          replyTo ! ResultRecorded(key, Some(this.successor))
        this
      case RecordKeyValuePairs(id, value, replyTo) =>
        context.log.info2("Node Hash reading {} with {}", value, id)
        lastKeyValueReading += id -> value
        replyTo ! KeyValuePairsRecorded(key, value)
        this
    }
  }
  override def onSignal: PartialFunction[Signal, Behavior[Command]] = {
    case PostStop =>
      this
  }
  def ithFinger_start(i: Int): BigInt = {
    val distance = Math.pow(2, i).toInt
    (n + BigInt(distance)) % max
  }
  def initFingerTable(): Unit = {
    for(i <- 0 until m){
      val start = ithFinger_start(i)
      fingerTable(i) = new FingerEntry(start, Interval(start, ithFinger_start(i + 1)), this)
    }
    predecessor = this          // Set predecessor to self
  }
  def closest_preceding_finger(id: BigInt): Node = {
    // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent, right?
    for (i <- m to 1) {                   // Scan what we are looking for from farthest to closest
      val start = ithFinger_start(i)
      val end = id
      val key = fingerTable(i).node.key
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
