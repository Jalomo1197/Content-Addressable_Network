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
  // Grab Node References     toProcess[Keys, nodeRef], right?
  case class receiveList(toProcess: Map[String, ActorRef[Node.Command]]) extends Command
  // To send to arbitrary node that has not been initialized
  case class initializeNode(processed: Int, toProcess: Map[String, ActorRef[Node.Command]], included: List[ActorRef[Node.Command]]) extends Command
  // Sent from nodes who have processed the initializeNode Command.
  case class updateFingerTable(processed: Int, included: List[ActorRef[Node.Command]]) extends Command
}
// m - bit Identifier (log base 2 of # of nodes) i.e (8 nodes yield a 3-bit modifier)
class Node(context: ActorContext[Node.Command], key: String, value: String, m: Int)
  extends AbstractBehavior[Node.Command](context){
  import Node._
  import Chord.keyLookup
  import User.queryResponse
  // Number of Nodes which are considered "active" within the hash ring
  var processed: Int = 0
  // Activated when the first node receives the list of actors from the User
  var nodes: List[ActorRef[Node.Command]] = List.empty[ActorRef[Node.Command]]
  var gotIt = false
  // Default predecessor
  var predecessor: ActorRef[Node.Command] = context.self
  // Largest m - bit value
  val max: Int = math.pow(2, m).toInt
  // Generate Hash Value for current Node (Use var n to maintain consistency with Research Publication)
  var n: Int = Hash.encrypt(key, m)
  // TODO: INCLUDED DOES NOT HAVE KEYS
  var nodeToHash: Map[ActorRef[Node.Command], Int] = Map.empty[ActorRef[Node.Command], Int]
  var hashToKey: Map[Int, String] = Map.empty[Int, String]
  // [49231231, "google.com"] [hash(key), value]
  var lastKeyValueReading: Map[Int, String] = Map.empty[Int, String]
  // This node adds itself to map
  nodeToHash += context.self -> n
  hashToKey += n -> key
  lastKeyValueReading += n -> value
  // Initialized when the Node receives list of actors from User
  var fingerTable: Array[FingerEntry] = new Array[FingerEntry](m)
  // Join
  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
        // TODO _1: Update finger table with those references in included
      case initializeNode(processed, toProcess, included) =>
        // NOTE: First two sections of Finger Table (start, interval) should already be calculated by now
        // NOTE: Last Node Actor reference in "included" STRICTLY IS the one who sent initializeNode Command

        // Iterate through "included" list and update successor node references up to "processed"
        for (node <- included.indices)

        // SCAN FROM TOP TO BOTTOM

        // Append this nodes reference to "included" (context.self)
        // Including this node now
        this.processed = processed + 1
        if (toProcess.nonEmpty){
          // Now select an arbitrary node ref from "toProcess"
          // Subtract that node from "toProcess"
          // Send initializeNode(this.processed, toProcess, included)
        }
        else{
          // NOTE: this is the last node
          // Send updateFingerTable(this.processed, included) to ALL Node Actor references in "included" (not to yourself (last index))
        }
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
        val interval = Interval(n + distance, nodeToHash(this.successor) + distance)
        val hash = Hash.encrypt(key, m)
        val value = lastKeyValueReading.getOrElse(nodeToHash(this.successor), "No Key Found")
        // Reachable
        if (interval.contains(hash)) {
          val closest_node = closest_preceding_finger(hash)
          val closest_hash = nodeToHash(closest_node)
          // Single Node
          if(hashToKey(closest_hash) == key)
            user ! queryResponse(key, Some(value))
          // No Key Found must call Find Predecessor and go to that Node
          else
            findPredecessor(hash) ! keyLookup(key, user)
        }
        // Found Key (More than One Node)
        else
          user ! queryResponse(key, Some(value))
        this
      case receiveList(toProcess) =>
        processed = 1
        val included: List[ActorRef[Node.Command]] = List(context.self)
        // Subtract this node's reference (context.self) from toProcess
        toProcess -= key
        if(toProcess.nonEmpty){
          // Now select an arbitrary node ref from "toProcess"
          val new_node_key = toProcess.keys.toList.head
          val new_node = toProcess(new_node_key)
          // Subtract that node from "toProcess"
          toProcess -= new_node_key
          new_node ! initializeNode(this.processed, toProcess, included)
        }
        else{
          // Set up of finger table
          initFingerTable()
          gotIt = true
          // Should I grab the keys here ??????
          this.nodes = toProcess.values.toList
        }
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
      fingerTable(i) = new FingerEntry(start, Interval(start, ithFinger_start(i + 1)), context.self)
    }
    // Set predecessor to self
    predecessor = context.self
  }
  def findPredecessor(id: Int): ActorRef[Node.Command] = {
      val distance = ithFinger_start(1)
      val successor_to_id = nodeToHash(this.successor)    // Successor guaranteed
      val interval = Interval(n + distance, successor_to_id + distance)
      // When id belongs to node's successor => we found the predecessor
      if(!interval.contains(id))
        return closest_preceding_finger(id)                   // Continue moving counter clockwise
    context.self                                              // Found Predecessor
  }
  def closest_preceding_finger(id: Int): ActorRef[Node.Command] = {
    // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent.
    // Scan what we are looking for from farthest to closest
    for (i <- m to 1) {
      val distance = ithFinger_start(1)
      val interval = Interval(n + distance, id)
      val preceding_ID_node = fingerTable(1).node      // Get Hash
      val preceding_ID = nodeToHash(preceding_ID_node)
      // Found node closest to what I'm looking for
      if (interval.contains(preceding_ID))
        return fingerTable(i).node
    }
    // current node is closest lol all that scanning for nothing (get fucked kid)
    context.self
  }
  // By definition the first entry is the successor
  def successor: ActorRef[Node.Command] =
    fingerTable(0).node
}