package Chord

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import response.{successor_type, predecessor_type,  updateOthers_type, initTable_type}

object Node {
  def apply(key: String, value: String, m: Int, hashedKey: Int): Behavior[Command] = {
    Behaviors.setup(context => new Node(context, key, value, m, hashedKey))
  }
  trait Command
  //case class update_others() extends Command

  case class init_table() extends Command
  case class set_predecessor(hashedKey: Int, currentNode: ActorRef[Node.Command]) extends Command
  case class find_successor_attempt(replyTo: ActorRef[Node.Command], currentNodeKey: Int, currentNodeSuccessor_id: Int, hashToNode: Map[Int, ActorRef[Node.Command]]) extends Command
  case class find_predecessor_attempt(insert_id: Int, queryType: response.Value, pred: ActorRef[Node.Command], predKey: Int, sucKey: Int) extends Command
  case class closest_preceding_finger(id: Int, replyTo: ActorRef[Node.Command], queryType: response.Value) extends Command
  case class closest_preceding_finger_response(id: Int) extends Command
  case class find_successor_of(new_node_key: Int, new_node: ActorRef[Node.Command]) extends Command

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
      //  Only new nodes process this command
      case join_1(managerNode) =>
        if (context.self.equals(managerNode)) // newNode is the only node in the network
          onlyNodeInNetwork()
        else                                  // else instruct manager node to find successor of this new node
          managerNode ! find_successor_of(this.hashedKey, context.self)
        this


      //  Only manager node processes this command
      case find_successor_of(new_node_key, new_node) =>
        this.hashToNode += new_node_key -> new_node   // Add new node to map, for reference.
        find_successor(new_node_key, new_node)        // find new_node's successor's hashedKey
        this

      // All node perspectives'
      case closest_preceding_finger(id, replyTo, queryType) =>
        // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent.
        // Scan what we are looking for from farthest to closest
        val interval = Interval(this.hashedKey + 1, id - 1) // equivalent to ( n,id ) range in algorithm
        for (i <- m to 1) {
          val potential_closest_preceding_finger = fingerTable(i).node
          val potential_closest_id = this.fingerTable(i).getInterval.get_end
          // Found node closest to what I'm looking for
          if (interval.contains(_id)){
            val closest_preceding_node = potential_closest_preceding_finger
            val closest_id = potential_closest_id
            if(i + 1 <= m){
              val closest_preceding_node_successor_key = this.fingerTable(i + 1).getInterval.get_end
              replyTo ! find_predecessor_attempt(id, queryType, closest_preceding_node, closest_id, closest_preceding_node_successor_key)
            }
            else
              replyTo ! find_predecessor_attempt(id, queryType, closest_preceding_node, -420, -69)
          }

        }

      case find_predecessor_attempt(insert_id, queryType,closest_preceding_node, closest_node_key, closest_node_successor_key) =>
        // Root call was from find_predecessor
        // AFTER BREAK POINT IN UPDATE OTHERS
        if (queryType ==  updateOthers_type){
          val interval = Interval(closest_node_key + 1, closest_node_successor_key)
          // When id belongs to node's successor => we found the predecessor
          if(!interval.contains(insert_id))    // Iteration of while loop in find_predecessor (algorithm)
            closest_preceding_node ! closest_preceding_finger(insert_id, context.self, predecessor_type)
          else {                               // else valid closest preceding node. Send update
            val i = find_index(this.hashedKey, insert_id)
            // essentially the last line in updateOther (algorithm) from the newNodes perspective
            closest_preceding_node ! updateFingerTable(context.self, i)
          }
        }
        // AFTER BREAK POINT IN INIT FINGER TABLE
        else if (queryType == initTable_type){
          context.self !
        }

        this


// NOT SORTED BELOW
      case join_2 (successor_hashedKey, hashToNode) =>
        this.hashToNode = hashToNode
        // now we set this (context.self) new nodes successor
        // Next we update
        update_others()
        this






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




        // current node is closest lol all that scanning for nothing (get fucked kid)
        //context.self
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
/* ***************************************************************************************************************
*               Functions for local changes and/or kick starting conversation for query answer
* ***************************************************************************************************************/
    // Purpose: Changes made to this nodes finger table
    def onlyNodeInNetwork(): Unit = {                                           // new node perspective
      for (i <- 1 to m) {
        this.fingerTable (i).node = context.self
        this.predecessor = context.self
      }
    }


    // Purpose_1: find predecessor of node (hashKey = id)
    def find_predecessor(id: Int): Unit = {
      val interval = Interval(this.hashedKey + 1, this.successor)
      if(!interval.contains(id)){ // start 'While loop' conversation
        context.self ! closest_preceding_finger(id, context.self,  updateOthers_type)
      }
      else                         // 'While loop' conversation never executed
        context.self ! find_predecessor_attempt(id, updateOthers_type,context.self, this.hashedKey, this.successor)
    }


    // Purpose_1: kick-starts conversation to find new nodes successor OR
    // Purpose_2: kick-starts conversation to find a node's (with hashKey being id) successor
    // replyTo is node that made query to manager node (so far seen cases: its been newNode)
    def find_successor(id: Int, replyTo: ActorRef[Node.Command ]): Unit = {     // manager node perspective
      // First iteration of while loop in find_predecessor (because find_successor calls find_predecessor)
      val interval = Interval(this.hashedKey + 1, this.successor)

      if(!interval.contains(id)){ // start 'While loop' conversation
        context.self ! closest_preceding_finger(id, replyTo, successor_type)
      }
      else                        // 'While loop' conversation never executed
        replyTo ! find_successor_attempt() //TODO
    }



  // NOT SORTED BELOW


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



  // By definition the first entry is the successor
  def successor: Int =
    this.fingerTable(1).getInterval.get_end


}

