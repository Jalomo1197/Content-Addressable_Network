package Chord

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import response.{successor_type, predecessor_type}

object Node {
  def apply(key: String, value: String, m: Int, hashedKey: Int): Behavior[Command] = {
    Behaviors.setup(context => new Node(context, key, value, m, hashedKey))
  }
  trait Command
  //case class update_others() extends Command
  case class set_predecessor(hashedKey: Int, currentNode: ActorRef[Node.Command]) extends Command
  case class find_successor_attempt(replyTo: ActorRef[Node.Command], currentNodeKey: Int, currentNodeSuccessor_id: Int, hashToNode: Map[Int, ActorRef[Node.Command]]) extends Command
  case class find_predecessor_attempt(predecessor_id: Int, currentNode: ActorRef[Node.Command], currentNodeKey: Int, currentNodeSuccessorKey: Int) extends Command
  case class closest_preceding_finger(id: Int, replyTo: ActorRef[Node.Command], queryType: response.Value) extends Command
  case class closest_preceding_finger_response(id: Int) extends Command
  case class init_finger_table(new_node_key: Int, new_node: ActorRef[Node.Command]) extends Command

  case class join_1(node: ActorRef[Node.Command]) extends Command
  case class join_2 (successorKey: Int , hashToNode: Map[Int, ActorRef[Node.Command]]) extends Command
   // Sent from nodes who have processed the initializeNode Command.
  case class updateFingerTable(s: ActorRef[Node.Command], i: Int) extends Command
}
// m - bit Identifier (log base 2 of # of nodes) i.e (8 nodes yield a 3-bit modifier)
class Node(context: ActorContext[Node.Command], key: String, value: String, m: Int, hashedKey: Int)
  extends AbstractBehavior[Node.Command](context){
  import Node._
  import Chord.keyLookup
  import User.queryResponse

  // Default predecessor
  var predecessor: ActorRef[Node.Command] = context.self
  // Largest m - bit value
  val max: Int = math.pow(2, m).toInt
  var nodeToHash: Map[ActorRef[Node.Command], Int] = Map.empty[ActorRef[Node.Command], Int]
  var hashToNode: Map[Int, ActorRef[Node.Command]] = Map.empty[Int, ActorRef[Node.Command]]
  // Initialized when the Node receives list of actors from User
  var fingerTable: Array[FingerEntry] = new Array[FingerEntry](m)


  // This node adds itself to map
  //nodeToHash += context.self -> hashToKey
  //hashToKey += hashedKey -> key


  // Join
  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
      case find_predecessor_attempt(predecessor_id, currentNode, currentNodeKey, currentNodeSuccessorKey) =>
        val interval = Interval(currentNodeKey + 1, currentNodeSuccessorKey)
        // When id belongs to node's successor => we found the predecessor
        if(!interval.contains(predecessor_id))
          currentNode ! closest_preceding_finger(predecessor_id, context.self, predecessor_type)
        else {
          val i = find_index(this.hashedKey, predecessor_id)
          currentNode ! updateFingerTable(context.self, i)
        }
      // New Node inserted as predecessor in Network for this node
      case set_predecessor(hashedKey, currentNode) =>
        this.predecessor = currentNode
        hashToNode += hashedKey -> currentNode

      case find_successor_attempt(replyTo, currentNodeKey, currentNodeSuccessor_id, hashToNode) =>
        this.hashToNode = hashToNode
        this.fingerTable(1).setNode(hashToNode(currentNodeSuccessor_id))
        this.predecessor = hashToNode(currentNodeKey)
        // context.self = new_node
        hashToNode(currentNodeSuccessor_id) ! set_predecessor(this.hashedKey, context.self)
      // s is the actor reference, but we need to add s_id to case class (s_id = hashedKey)

      case updateFingerTable(s, i) =>
        val n = this.hashedKey
        hashToNode += s_id -> s
        val interval = new Interval(n, fingerTable(i).getInterval.get_end - 1)
        if (interval.contains(s_id)) {
          fingerTable(i).node = s
          this.predecessor ! updateFingerTable(s, i)
        }
        this


      case join_1(managerNode) =>
        if (context.self.equals(managerNode)){ // newNode is the only node in the network
          for (i <- 1 to m)
            this.fingerTable(i).node = context.self
          this.predecessor = context.self
        }
        else
          managerNode ! init_finger_table(this.hashedKey, context.self)
        this
      case join_2 (successor_hashedKey, hashToNode) =>
        this.hashToNode = hashToNode
        // now we set this (context.self) new nodes successor
        // Next we update
        update_others()
        this


        // Manager is the only one getting these
      case init_finger_table(new_node_key, new_node) =>
        this.hashToNode += new_node_key -> new_node
        // find new_node's successor's hashedKey
        find_successor(new_node_key, new_node)

        this


      case init_finger_table_2()=>
        new_node ! join_2 (successor_hashedKey, this.hashToNode)
        // TODO _1: Update finger table with those references in included
      case closest_preceding_finger(id, replyTo, queryType) =>
        // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent.
        // Scan what we are looking for from farthest to closest
        for (i <- m to 1) {
          val interval = Interval(this.hashedKey + 1, id - 1)
          val current_node = fingerTable(i).node
          val current_node_id = this.fingerTable(i).getInterval.get_end
          // Found node closest to what I'm looking for
          if (interval.contains(current_node_id)){
            val closest = current_node
            if(successor_type == queryType){
                // Found
              if(i + 1 <= m){
                val current_node_successor_id = this.fingerTable(i + 1).getInterval.get_end
                replyTo ! find_successor_attempt(id, closest, current_node_id, this.nodeToHash)
            }
            else if(predecessor_type == queryType){
              // replyTo : newNode

              if(i + 1 <= m){
                val current_node_successor_id = this.fingerTable(i + 1).getInterval.get_end
                replyTo ! find_predecessor_attempt(id, closest, current_node_id, current_node_successor_id)
              }
              // Fail
              replyTo ! find_predecessor_attempt(id, closest, -420, -69)
            }

          }


        }
        // current node is closest lol all that scanning for nothing (get fucked kid)
        context.self
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
  // Used in Find Predecessor to avoid passing i from closest_preceeding_finger
  def find_index(a: Int, b: Int): Int =
    1 + (Math.log(a - b) / Math.log(2)).toInt
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

  // Manager
  // id is newNode.hashkey
  //context.self is managerNode
  // replyTo is newNode
  def find_successor(id: Int, replyTo: ActorRef[Node.Command ]): Unit = {
    val interval = Interval(this.hashedKey + 1, this.successor)
    if(!interval.contains(id)){
      context.self ! closest_preceding_finger(id, replyTo, successor_type)
    }
    else
      replyTo ! find_successor_attempt()
  }
  def init_finger_table(id: Int): Unit = {
    for(i <- 1 until m){
      val interval = Interval(this.hashedKey, fingerTable(i).getInterval.get_end - 1)
      if(interval.contains(fingerTable(i + 1).getStart))
        fingerTable(i + 1).setNode(fingerTable(i).node)
      else {
        val index = fingerTable(i + 1).getStart
        fingerTable(i + 1).node ! find_successor(index, i)
      }
    }

    }
  }
  // context.self is newNode
  def find_predecessor(id: Int): Unit = {
    val interval = Interval(this.hashedKey + 1, this.successor)
    if(!interval.contains(id)){
      context.self ! closest_preceding_finger(id, context.self, predecessor_type)
    }
    // Found it!
    else
      context.self ! find_predecessor_attempt(id, context.self, this.hashedKey, this.successor)
  }

  // By definition the first entry is the successor
  def successor: Int =
    this.fingerTable(1).getInterval.get_end


}

