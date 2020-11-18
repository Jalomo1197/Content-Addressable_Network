package Chord

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors

object Node {
  def apply(key: String, value: String, m: Int, hashedKey: Int): Behavior[Command] = {
    Behaviors.setup(context => new Node(context, key, value, m, hashedKey))
  }
  trait Command
  //case class update_others() extends Command
  //case class findPredecessor(id: Int) extends Command
  case class closest_preceding_finger(id: Int, replyTo: ActorRef[Node.Command]) extends Command
  case class closest_preceding_finger_response(id: Int) extends Command
  case class init_finger_table(new_node_key: Int, new_node: ActorRef[Node.Command]) extends Command
  case class join(node: ActorRef[Node.Command]) extends Command
  case class joinResponse (successorKey: Int , hashToNode: Map[Int, ActorRef[Node.Command]]) extends Command
   // Sent from nodes who have processed the initializeNode Command.
  case class updateFingerTable(s: ActorRef[Node.Command], s_id: Int, i: Int) extends Command
}
// m - bit Identifier (log base 2 of # of nodes) i.e (8 nodes yield a 3-bit modifier)
class Node(context: ActorContext[Node.Command], key: String, value: String, m: Int, hashedKey: Int)
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
  // TODO: INCLUDED DOES NOT HAVE KEYS
  var nodeToHash: Map[ActorRef[Node.Command], Int] = Map.empty[ActorRef[Node.Command], Int]
  var hashToNode: Map[Int, ActorRef[Node.Command]] = Map.empty[Int, ActorRef[Node.Command]]
  var hashToKey: Map[Int, String] = Map.empty[Int, String]
  // [49231231, "google.com"] [hash(key), value]
  var lastKeyValueReading: Map[Int, String] = Map.empty[Int, String]
  // This node adds itself to map
  nodeToHash += context.self -> hashToKey
  hashToKey += hashedKey -> key
  lastKeyValueReading += hashedKey -> value
  // Initialized when the Node receives list of actors from User
  var fingerTable: Array[FingerEntry] = new Array[FingerEntry](m)
  // Join
  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
      // s is the actor reference, but we need to add s_id to case class (s_id = hashedKey)
      case updateFingerTable(s, s_id, i) =>
        val n = this.hashedKey
        hashToNode += s_id -> s
        val interval = new Interval(n, fingerTable(i).getInterval.get_end - 1)
        if (interval.contains(s_id)) {
          fingerTable(i).node = s
          this.predecessor ! updateFingerTable(s,s_id,i)
        }
        this
      case join(managerNode) =>
        if (context.self.equals(managerNode)){

        }
        else
          managerNode ! init_finger_table(this.hashedKey, context.self)


        this
      case joinResponse (successor_hashedKey, hashToNode) =>
        this.hashToNode = hashToNode
        // now we set this (context.self) new nodes successor
        // Next we update
        update_others()
        this

        // Manager is the only one getting these
      case init_finger_table(new_node_key, new_node) =>
        this.hashToNode += new_node_key -> new_node
        // find new_node's successor's hashedKey
        new_node ! joinResponse (, this.hashToNode)
        this
        // TODO _1: Update finger table with those references in included
      case closest_preceding_finger(id: Int, ) =>
        // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent.
        // Scan what we are looking for from farthest to closest
        for (i <- m to 1) {
          val interval = Interval(this.hashedKey + 1, id - 1)
          val preceding_ID_node = fingerTable(1).node      // Get Hash
          val preceding_ID = nodeToHash(preceding_ID_node)
          // Found node closest to what I'm looking for
          if (interval.contains(preceding_ID))
             // id_ref ! fingerTable(i).node
        }
        // current node is closest lol all that scanning for nothing (get fucked kid)
        context.self

      case closest_preceding_finger_response(id: Int) =>

      case keyLookup(key, user) =>
        val distance = ithFinger_start(1)
        val interval = Interval(hashedKey + distance, nodeToHash(this.successor) + distance)
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
          else {
            findPredecessor(hash) ! keyLookup(key, user)
          }
        }
        // Found Key (More than One Node)
        else
          user ! queryResponse(key, Some(value))
        this
    }

  }
  override def onSignal: PartialFunction[Signal, Behavior[Command]] = {
    case PostStop =>
      this
  }
  def update_others(): Unit = {
    for(i <- 1 to m){
      val distance = ithFinger_start(i - 1)
      val p = findPredecessor(this.hashedKey - distance)
      p ! updateFingerTable(context.self, this.hashedKey, i)
    }
  }
  //*update all nodes whose finger
  //*tables should refer to n
  //    n.update_others()
  //      for i = 1 to m
  //*find last node p whose ith finger might be n
  //      p = find_predecessor(n — 2^(i-1))
  //      p.update_finger_table(n, i);


  def ithFinger_start(i: Int): Int = {
    val distance = Math.pow(2, i).toInt
    (hashedKey + distance) % max
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
      val interval = Interval(hashedKey + distance, successor_to_id + distance)
      // When id belongs to node's successor => we found the predecessor
      if(!interval.contains(id))
        return closest_preceding_finger(id)                   // Continue moving counter clockwise
    context.self                                              // Found Predecessor
  }
  def find_succesor(hashedKey: Int) = {
    val n = findPredecessor(hashedKey)

  }
  // By definition the first entry is the successor
  def successor: ActorRef[Node.Command] =
    fingerTable(0).node

}