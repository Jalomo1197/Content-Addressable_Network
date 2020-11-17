package Chord

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors

object Node {
  def apply(key: String, value: String, m: Int): Behavior[Command] = {
    Behaviors.setup(context => new Node(context, key, value, m))
  }
  trait Command
  // Grab Node References
  case class receiveList(toProcess: Map[String, ActorRef[Node.Command]]) extends Command
  // To send to arbitrary node that has not been initialized
  case class initializeNode(processed: Int, toProcess: Map[String, ActorRef[Node.Command]], included: List[ActorRef[Node.Command]]) extends Command
  // Sent from nodes who have processed the initializeNode Command.
  case class updateFingerTable(processed: Int, included: List[ActorRef[Node.Command]]) extends Command
}

class Node(context: ActorContext[Node.Command], key: String, value: String, m: Int)
  extends AbstractBehavior[Node.Command](context){
  import Node._
  import Chord.keyLookup
  import User.queryResponse
  // TODO: // nodeToHash +=  n -> context.self

  var processed: Int = 0
  // Key to store (i.e. 73.51.227.5)
  var _key: String = key
  // m - bit Identifier (# of nodes)
  var m: Int = 0
  // encrypt k here
  var k: Int = 0
  var gotIt = false
  // Default predecessor
  var predecessor: ActorRef[Node.Command] = context.self
  // Largest m - bit value
  val max: Int = math.pow(2, m).toInt
  // Generate Hash Value for current Node (Use var n to maintain consistency with Research Publication)
  var n: Int = Hash.encrypt(key, m)
  var nodes: Iterable[ActorRef[Node.Command]] = Iterable.empty[ActorRef[Node.Command]]
  // [49231231, "google.com"] [hash(key), value]
  var lastKeyValueReading: Map[Int, String] = Map.empty[Int, String]
  lastKeyValueReading += n -> value
  var hashToKey: Map[Int, String] = Map.empty[Int, String]
  hashToKey += n -> key
  // TODO Question: class FingerEntry(start: Int, interval: Interval, node: Node), Should "node" be ActorRef[Node.Command] instead of class Node?
  var fingerTable: Array[FingerEntry] = new Array[FingerEntry](m)
  // Set up of finger table
  initFingerTable()
  // Can just call initFingerTable but need to declare it scala wise
  // Join
  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
        // TODO _1: Update finger table with those references in included
      case initializeNode(processed, toProcess, included) =>
        // NOTE: First two sections of Finger Table (start, interval) should already be calculated by now
        // NOTE: Last Node Actor reference in "included" STRICTLY IS the one who sent initializeNode Command

        // Iterate through "included" list and update successor node references up to "processed"
        // SCAN FROM TOP TO BOTTOM

        // Append this nodes reference to "included" (context.self)
        this.processed = processed + 1 // Including this node now

        // If "toProcess" is NOT empty
          // Now select an arbitrary node ref from "toProcess"
          // Subtract that node from "toProcess"
          // Send initializeNode(this.processed, toProcess, included)

        // When "toProcess" is empty
          // NOTE: this is the last node

        // Send updateFingerTable(this.processed, included) to ALL Node Actor references in "included" (not to yourself (last index))
        this

      case updateFingerTable(processed, included) =>
        // TODO _3: Iterate through finger table up to update successor node reference
        if (this.processed < processed){
          this.processed = processed
          // TODO _3
        }
        // Else we ignore update messaged because we have already updated successor node references pass "processed: Int" in message
        this

      case keyLookup(key, user) =>
        val distance = ithFinger_start(1)
        val interval = Interval(n + distance, this.successor.n + distance)
        val hash = Hash.encrypt(key, m)
        val value = lastKeyValueReading.getOrElse(this.successor.n, "No Key Found")
        // Reachable
        if (interval.contains(hash)) {
          val closest = closest_preceding_finger(hash)
          // Single Node
          if(closest.key == key)
            user ! queryResponse(key, Some(value))
          // No Key Found must call Find Predecessor and go to that Node
          else
            findPredecessor(hash).context.self ! keyLookup(key, user)
        }
        // Found Key (More than One Node)
        else
          user ! queryResponse(key, Some(value))
        this
      // see TODO BIG: initFingerTable()
      case receiveList(toProcess) =>
        /* THESE ARE PREVIOUS NOTES, left for reference can ignore now
        // In regards to TODO BIG
        // Don't you have to do a for loop and for though all node refs to send updated "m" and list of node Refs? (new case class (m, list, includedList))
        */

        // NOTE: finger table should be constructed by now

        // Set up "included" list just having context.self
        // processed = 1;

        // Subtract this node's reference (context.self) from toProcess

        // If "toProcess" is NOT empty
          // Now select an arbitrary node ref from "toProcess"
          // Subtract that node from "toProcess"
          // Send initializeNode(this.processed, toProcess, included)

        // When "toProcess" is empty
          // NOTE: this is the last node

        gotIt = true
        nodes = toProcess.values
        this.m = nodes.size
        this
    }
  }
  override def onSignal: PartialFunction[Signal, Behavior[Command]] = {
    case PostStop =>
      this
  }
  def ithFinger_start(i: Int): Int = {
    val distance = Math.pow(2, i).toInt
    (n + distance) % max
  }
  def initFingerTable(): Unit = {
    // TODO BIG: Question: the first node has m = 0, this loop therefore does not execute, part of algorithm?
    // Or does the next node construction tell this node to update? If so where do you send this node ref to other nodes? and where in the code
    // do other nodes send updates to this node?
    for(i <- 1 until m){
      val start = ithFinger_start(i)
      fingerTable(i) = new FingerEntry(start, Interval(start, ithFinger_start(i + 1)), this)
    }
    // Set predecessor to self
    predecessor = context.self
  }
  /*
  *   Is it guaranteed the  list of references I receive from Chord are the same Nodes reference the Nodes have of themselves?
   */
  def findPredecessor(id: Int): Node = {
      val distance = ithFinger_start(1)
      val interval = Interval(n + distance, this.successor.n + distance)
      // When id belongs to node's successor => we found the predecessor
      if(!interval.contains(id))
        return closest_preceding_finger(id)       // Continue moving counter clockwise
    this                                          // Found Predecessor
  }
  def closest_preceding_finger(id: Int): Node = {
    // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent.
    // Scan what we are looking for from farthest to closest
    for (i <- m to 1) {
      val distance = ithFinger_start(1)
      val interval = Interval(n + distance, id)
      val preceding_ID = fingerTable(1).node.n
      // Found node closest to what I'm looking for
      if (interval.contains(preceding_ID))
        return fingerTable(i).node
    }
    // current node is closest lol all that scanning for nothing (get fucked kid)
    this
  }
  // By definition the first entry is the successor
  def successor: Node =
    fingerTable(0).node
}
