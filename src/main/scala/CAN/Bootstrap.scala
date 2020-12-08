package CAN


import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

import scala.util.Random

object Bootstrap{
  def apply():  Behavior[Command] = Behaviors.setup(context => new Bootstrap(context))

  trait Command

  final case object initializeZones extends Command
  case class getNodeInNetwork(p: Procedure[Node.Command]) extends Command
}

class Bootstrap(context: ActorContext[Bootstrap.Command]) extends AbstractBehavior[Bootstrap.Command](context){
  import Bootstrap._
  import Node.acquiredNodeInNetwork

  var zone_count = 0
  var active_nodes: List[ActorRef[Node.Command]] = List.empty[ActorRef[Node.Command]]
  // Assign Zones
  // Assign Neighbors

  override def onMessage(msg: Bootstrap.Command): Behavior[Bootstrap.Command] = {
<<<<<<< HEAD
    msg match {
      case initializeZones =>
        initializeNeighbors()
        this.zone_count += 4
        this
      case getNodeInNetwork(p) =>
        // Randomly choose node from bootstrap node
        val node = getRandomNode(active_nodes, new Random())
        p.getReplyTo.get ! acquiredNodeInNetwork(Procedure[Node.Command]().withReference(active_nodes.head))
        this
    }
=======
  msg match {
    case initializeZones =>
      initializeNeighbors()
      this.zone_count += 4
      this
    case getNodeInNetwork(p) =>
      // Randomly choose node from bootstrap node
      val node: Node.Command = getRandomNode(active_nodes, new Random())
      /*
      *   Contact DNS
      *   Bootstrap reply
      *   Query bootstrap to get random node
      *
      * */
      // Choose point P of identifier for new Node

      this
  }
>>>>>>> routing
  }


  def initializeNeighbors():Unit = {
    import Node.{setZone,initializeNeighbors}
    var initialZones = List( Zone((0, 7), (0, 7)) , Zone((7, 15), (0, 7)), Zone((0, 7), (7, 15)), Zone((7, 15), (7, 15)))

    /* Spawn New Nodes */
    for(i <- 0 until 4){
      val new_node = context.spawn(Node(),s"CAN-node-$i")
      active_nodes +:= new_node
      new_node ! setZone(initialZones(i))
    }

    active_nodes.foreach(node => {
      node ! initializeNeighbors(active_nodes)
    })


    /* Zones must acknowledge each other as neighbors */
    //zone.set_neighbors(List(zone2, zone3))
    //zone2.set_neighbors(List(zone, zone4))
    //zone3.set_neighbors(List(zone, zone4))
    //zone4.set_neighbors(List(zone2, zone3))
  }
<<<<<<< HEAD

=======
>>>>>>> routing
  def getRandomNode[A](seq: Seq[A], random: Random): A =
    seq(random.nextInt(seq.length))
}

