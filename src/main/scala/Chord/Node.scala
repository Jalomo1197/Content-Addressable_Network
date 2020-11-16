package Chord


import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
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
  // Key to store (i.e. 73.51.227.5)
  var _key: String = key
  // m - bit Identifier
  var m: Int = 0
  // encrypt k here
  var k: Int = 0
  var gotIt = false
  // Default predecessor
  var predecessor: Node = this
  // Largest m - bit value
  val max: Int = math.pow(2, m).toInt
  // Generate Hash Value for current Node
  var n: Int = Hash.encrypt(key, m)
  var nodes: Iterable[ActorRef[Node.Command]] = Iterable.empty[ActorRef[Node.Command]]
  var lastKeyValueReading: Map[Int, String] = Map.empty[Int, String]
  // [49231231, "google.com"]
  lastKeyValueReading += n -> value
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
        val hash = Hash.encrypt(key, m)
        // Reachable
        if (interval.contains(hash)) {
          val closest = closest_preceding_finger(hash)
          val value = lastKeyValueReading.getOrElse(this.successor.n, "No Key Found")
          if(closest.key == key)                                // ONE Node
            user ! queryResponse(key, Some(value))
          //else
            //FindSuccessor(key, replyTo)
        }
        else {
          val value = lastKeyValueReading.getOrElse(this.n, "No key found")
          user ! queryResponse(key, Some(value))
        }
        this
      case receiveList(actors: Map[String, ActorRef[Node.Command]]) =>
        gotIt = true
        nodes = actors.values
        m = nodes.size
        this
      // Write
      case FindPredecessor(key, replyTo) =>
        if(key == this.key){
          //lastKeyValueReading += key -> this.n
        }
        // This is what we are looking for
        val k = Hash.encrypt(key, m)
        val start = fingerTable(0).getStart
        val end = this.successor.fingerTable(0).getInterval.get_end
        val interval = Interval(start, end)
        if (interval.contains(k)){                                         // Reachable
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
        if (interval.contains(k)) {                                    // Reachable
          val closest = closest_preceding_finger(k)
          return closest
        }
        this
    }
  }
  override def onSignal: PartialFunction[Signal, Behavior[Command]] = {
    case PostStop =>
      this
  }
  def FindSuccessor(key: String): Option[ActorRef[Node.Command]] = {
    // Check within range
    val start = fingerTable(0).getStart
    val end = this.successor.fingerTable(0).getInterval.get_end
    val interval = Interval(start, end)
    if (interval.contains(k)) {                                    // Reachable
      val closest = closest_preceding_finger(k)
      if(closest.key == key)                                // ONE Node
        this.successor
      else
        None
    }
    Some(context.self)
  }
  def ithFinger_start(i: Int): Int = {
    val distance = Math.pow(2, i).toInt
    (n + distance) % max
  }
  def initFingerTable(): Unit = {
    for(i <- 1 until m){
      val start = ithFinger_start(i)
      fingerTable(i) = new FingerEntry(start, Interval(start, ithFinger_start(i + 1)), this)
    }
    predecessor = this          // Set predecessor to self
  }
  def closest_preceding_finger(id: Int): Node = {
    // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent, right?
    for (i <- m to 1) {                   // Scan what we are looking for from farthest to closest
      val start = ithFinger_start(i)
      val end = id
      val key = fingerTable(i).node.key
      val k = Hash.encrypt(key, m)
      val interval = Interval(start, end)
      if (interval.contains(k))
        return fingerTable(i).node            // Found node closest to what I'm looking for
    }
    this                                      // current node is closest lol all that scanning for nothing (get fucked kid)
  }
  // By definition the first entry is the successor
  def successor: Node =
    fingerTable(0).node
}
