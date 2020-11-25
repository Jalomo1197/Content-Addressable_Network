package Chord_Algo

import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal}
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import response.{FT_entry_update, initTable_type, predecessor_type, successor_type, updateOthers_type, key_response}
import scala.math.pow

object Node {
  def apply(n_id: Int, m: Int): Behavior[Command] = {
    Behaviors.setup(context => new Node(context, n_id, m))
  }
  trait Command
  // Found Key!
  case class replyToUser(user: ActorRef[User.Command]) extends Command
  // new nodes perspective
  case class init_table(pred: ActorRef[Node.Command], predKey: Int, sucKey: Int, hashToNode: Map[Int ,ActorRef[Node.Command]]) extends Command
  // every nodes perspective
  case class set_predecessor(hashedKey: Int, currentNode: ActorRef[Node.Command]) extends Command
  // new nodes perspective
  case class find_predecessor_attempt(insert_id: Int, queryType: response.Value, pred: ActorRef[Node.Command], predKey: Int, sucKey: Int, hashToNode: Map[Int, ActorRef[Node.Command]],  updateIndex: Int, user: ActorRef[User.Command] = null) extends Command
  // every nodes perspective
  case class closest_preceding_finger(id: Int, replyTo: ActorRef[Node.Command], queryType: response.Value, hashToNode: Map[Int, ActorRef[Node.Command]], updateIndex: Int, user: ActorRef[User.Command] = null) extends Command
  // manager nodes perspective
  case class find_successor_of(new_id: Int, new_node: ActorRef[Node.Command], queryType: response.Value, updateIndex: Int = -1) extends Command
  // new nodes perspective
  case class join(node: ActorRef[Node.Command]) extends Command
  // Sent from nodes who have processed the initializeNode Command.
  case class updateFingerTable(s: ActorRef[Node.Command], s_id: Int, i: Int) extends Command
}

// m - bit Identifier (log base 2 of # of nodes) i.e (8 nodes yield a 3-bit modifier)
class Node(context: ActorContext[Node.Command], n_id: Int, m: Int)
  extends AbstractBehavior[Node.Command](context){
  import Node._
  import Chord.keyLookup
  import User.queryResponse
  // Default predecessor: only used in first node
  var predecessor: ActorRef[Node.Command] = context.self
  val n_ref: ActorRef[Node.Command] = context.self
  // Largest m - bit value
  val max: Int = math.pow(2, m).toInt
  var nodeToHash: Map[ActorRef[Node.Command], Int] = Map.empty[ActorRef[Node.Command], Int]
  // reference to node actor, given nodes id (n_id)
  var reference: Map[Int, ActorRef[Node.Command]] = Map.empty[Int, ActorRef[Node.Command]]
  // Initialized finger table columns: START and Interval
  var fingerTable: FingerTable = FingerTable(n_id, m)
  // Access to manager node for querying
  var managerNode : Option[ActorRef[Node.Command]] = None


  // Commands node actors respond to
  override def onMessage(msg: Command): Behavior[Command] = {
    msg match {
      // Every new nodes' first command
      case join(managerNode) =>
        context.log.info("Node: "+ n_id + " requesting join" )
        // Instruct manager node to find successor of this new node
        if (!context.self.equals(managerNode))
          managerNode ! find_successor_of(n_id, n_ref, initTable_type)
        // This node is the only node in the network
        else{
          context.log.info("Node: "+ n_id + " is the first node in the network" )
          fingerTable.initializeNodeColumn(n_ref)
          predecessor = context.self
        }
        this.managerNode = Some(managerNode)


      // Only manager node processes this command, q_id = the n_id of the node in question (query_id)
      case find_successor_of(q_id, replyTo, queryType, updateIndex) =>
        // Save new node actor reference
        // reference += q_id -> new_ref TODO: cannot do this here because q_id & new_ref may not be the same node FIX: tell manger node to update reference from Chord
        // find new node's successor's id (s_id), to eventually be sent back to the new node
        find_successor(q_id, replyTo, queryType, reference, updateIndex)


      // All node perspectives'
      case closest_preceding_finger(id, replyTo, queryType, hashToNode, updateIndex, usr) =>
        // Note: Research paper says from m to 1 but (n, id) inclusive range => so exclusive range is equivalent.
        // Scan what we are looking for from farthest to closest
        val interval = Interval(this.n_id + 1, id - 1) // equivalent to ( n,id ) range in algorithm
        for (i <- m to 1) {
          val potential_closest_preceding_finger = fingerTable(i).node
          val potential_closest_id = this.fingerTable(i).getInterval.get_end
          // Found node closest to what I'm looking for
          if (interval.contains(potential_closest_id)){
            val closest_preceding_node = potential_closest_preceding_finger
            val closest_id = potential_closest_id
            if(i + 1 <= m){
              val closest_preceding_node_successor_key = this.fingerTable(i + 1).getInterval.get_end
              replyTo ! find_predecessor_attempt(id, queryType, closest_preceding_node, closest_id, closest_preceding_node_successor_key, hashToNode,  updateIndex, usr)
            }
            else
              replyTo ! find_predecessor_attempt(id, queryType, closest_preceding_node, -420, -69, hashToNode,  updateIndex, usr)
          }
        }


      case find_predecessor_attempt(insert_id, queryType,closest_preceding_node, closest_node_key, closest_node_successor_key, hashToNode,  updateIndex, usr) =>
        // Root call was from find_predecessor
        // AFTER BREAK POINT IN UPDATE OTHERS
        if (queryType ==  updateOthers_type){
          val interval = Interval(closest_node_key + 1, closest_node_successor_key)
          // When id belongs to node's successor => we found the predecessor
          if(!interval.contains(insert_id)) { // Iteration of while loop in find_predecessor (algorithm)
            context.log.info("New Node: " +context.self.path.name + " predecessor not found. Now asking node: " + closest_preceding_node.path.name )
            closest_preceding_node ! closest_preceding_finger(insert_id, context.self, queryType, hashToNode, updateIndex)
          }
          else {                               // else valid closest preceding node. Send update
            context.log.info("New Node: " + context.self.path.name + " found predecessor.")
            context.log.info("New Node: " + context.self.path.name + " is sending predecessor: "+ closest_preceding_node.path.name + " update finger table command")
            val i = find_index(this.n_id, insert_id)
            // essentially the last line in updateOther (algorithm) from the newNodes perspective
            closest_preceding_node ! updateFingerTable(context.self, this.n_id, i)
          }
        }
        // AFTER BREAK POINT IN INIT FINGER TABLE
        else if (queryType == initTable_type) {
          context.log.info("New Node: " + context.self.path.name + " knows predecessor ("+closest_preceding_node.path.name+") and successor("+hashToNode(closest_node_successor_key).path.name+")")
          context.self ! init_table(closest_preceding_node, closest_node_key,  closest_node_successor_key, hashToNode)
         }
        else if (queryType == FT_entry_update){
          // assign that i+1 to hashNode(closest_node_successor_key)
          fingerTable(updateIndex).setNode(hashToNode(closest_node_successor_key))
        }
        else if (queryType == key_response){
          hashToNode(closest_node_successor_key) ! replyToUser(usr)

        }


      case init_table(pred, predKey, sucKey, hashToNode) =>
        val _successor = hashToNode(sucKey)
        // Update Finger Table Successor
        fingerTable(1).node = _successor
        fingerTable(1).getInterval.set_end(sucKey)
        this.predecessor = pred
        // Setting predecessor of this node's successor
        _successor ! set_predecessor(this.n_id, context.self)
        for(i <- 1 until m){
          val interval = new Interval(this.fingerTable(i).getStart + 1, fingerTable(i).getInterval.get_end - 1)
          if(interval.contains(fingerTable(i + 1).getStart)){
            fingerTable(i + 1).setNode(fingerTable(i).node)
          }
          // Send to ManagerNode
          else{
            this.managerNode.get ! find_successor_of(fingerTable(i + 1).getStart, hashToNode(fingerTable(i + 1).getStart), FT_entry_update, i+1)
            fingerTable(i + 1).setNode(hashToNode(fingerTable(i + 1).getStart))
          }

        }


      // New Node inserted as predecessor in Network for this node
      case set_predecessor(hashedKey, currentNode) =>
        this.predecessor = currentNode
        reference += hashedKey -> currentNode



      case updateFingerTable(s, s_id, i) =>
        val n = this.n_id
        reference += s_id -> s
        val interval = new Interval(n, fingerTable(i).getInterval.get_end - 1)
        // Set the interval for the ith Entry
        fingerTable(i).setInterval(interval)
        // DEBUGGING
        context.log.info("Interval for NodeID: " + n.toString + "\n" + fingerTable(i).printInterval)
        if (interval.contains(s_id)) {
          fingerTable(i).node = s
          context.log.info("Node: " + context.self.path.name + " is sending predecessor: "+ this.predecessor.path.name + " update finger table command")
          this.predecessor ! updateFingerTable(s, s_id,i)
        }


      // manager node perspective
      case keyLookup(key, user) =>
        val hash = Hash.encrypt(key, m)
        // Found Key
        if (this.n_id == hash)
          user ! queryResponse(key, Some(value))
        else
          find_successor(hash, context.self, key_response, null, -1, user)

      case replyToUser(user) =>
        user ! queryResponse(this.key, Some(this.value))
    }
    this
    // END OF ON_MESSAGE
  }
  override def onSignal: PartialFunction[Signal, Behavior[Command]] = {
    case PostStop =>
      this
  }
/* ***************************************************************************************************************
*               Functions for local changes and/or kick starting conversation for query answer
* ***************************************************************************************************************/

    // Purpose_1: kick-starts conversation to find new nodes successor OR
    // Purpose_2: kick-starts conversation to find a node's (with q_id being that nodes n_id) successor
    // Note: replyTo is node that made query to manager node
    def find_successor(q_id: Int, replyTo: ActorRef[Node.Command ], queryType: response.Value, managerReferenceMap: Map[Int, ActorRef[Node.Command]] = null, updateIndex: Int = -1, user: ActorRef[User.Command] = null): Unit = {     // manager node perspective
      context.log.info("Attempting to find successor of Node: " + q_id + " Query Type: " + queryType)
      find_predecessor(q_id, replyTo, queryType, managerReferenceMap, updateIndex, user) // calls find_predecessor like in algorithm
    }


    // Purpose_1: find predecessor of node: q_id
    def find_predecessor(q_id: Int, replyTo: ActorRef[Node.Command ], queryType: response.Value, managerReferenceMap: Map[Int, ActorRef[Node.Command]], updateIndex: Int = -1, user: ActorRef[User.Command] = null): Unit = {
      context.log.info("Attempting to find predecessor of Node: " + q_id + " Query Type: " + queryType)
      val interval = Interval(this.n_id + 1, this.successor)
      context.log.info(" interval " +this.n_id +" " + this.successor)
      if(!interval.contains(q_id)){ // start 'While loop' conversation
        context.self ! closest_preceding_finger(q_id, replyTo,  queryType, managerReferenceMap, updateIndex, user)
      }
      else                         // 'While loop' conversation never executed
        replyTo ! find_predecessor_attempt(q_id, queryType,replyTo, this.n_id, this.successor, managerReferenceMap, updateIndex, user )
    }





  // NOT SORTED BELOW
  def update_others(): Unit = {
    for(i <- 1 to m){
      val distance = ithFinger_start(i - 1)
      find_predecessor(this.n_id - distance, context.self, updateOthers_type, reference)
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
    (n_id + distance) % max
  }

  // Used in Find Predecessor to avoid passing i from closest_preceeding_finger
  def find_index(a: Int, b: Int): Int =
    1 + (Math.log(a - b) / Math.log(2)).toInt

  // By definition the first entry is the successor
  def successor: Int =
    this.fingerTable(1).getInterval.get_end

}