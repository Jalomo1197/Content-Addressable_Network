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
  import Node.{acquiredNodeInNetwork,findZone}
  import DNS.insert

  var zone_count = 0
  var active_nodes: List[ActorRef[Node.Command]] = List.empty[ActorRef[Node.Command]]
  // Assign Zones
  // Assign Neighbors

  override def onMessage(msg: Bootstrap.Command): Behavior[Bootstrap.Command] = {
    msg match {

      case initializeZones =>
        initializeNeighbors()
        this.zone_count += 4
        this
      case getNodeInNetwork(p) =>
        // Randomly choose node from bootstrap node
        val node = getRandomNode(active_nodes, new Random())
        p.getReplyTo.get ! acquiredNodeInNetwork(Procedure[Node.Command]().withReference(active_nodes.head))
        context.log.info(this.getClass +" : getRandomNode(p) => ")
        this
      case insert(kv) =>
        active_nodes.foreach(a => a ! findZone (kv))
        context.log.info(this.getClass +" : inserting(kv) => Node" + kv.toString)
        this
    }
  }

  def initializeNeighbors():Unit = {
    import Node.{setZone,initializeNeighbors}

    // init 16x16 conceptual grid into 4 coordinate planes ( + ) where center of + is ( x = 7, y = 7 )
    // Self defined circular coordinate plane ( + ) meaning List (1) would be top left, then (2) right, then (3) below 1 and 4 below 2
    //    1 = (0,7),(0,7)  -> (x,y) = (0,0),(7,7)
    //    2 = (7,15),(0,7) -> (x,y) = (7,0),(15,7)
    //    3 = (0,7),(7,15) -> (x,y) = (0,7),(7,15)
    //    4 = (7,15),(7,15)-> (x,y) = (7,7),(15,15)
    var initialZones = List( Zone((0, 7), (0, 7)) , Zone((7, 15), (0, 7)), Zone((0, 7), (7, 15)), Zone((7, 15), (7, 15)))

    /* Spawn New Nodes */
    for(i <- 0 until 4){
      val new_node = context.spawn(Node(),s"CAN-node-$i")
      active_nodes +:= new_node
      new_node ! setZone(Procedure[Node.Command]().withZone(initialZones(i)))
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
  def getRandomNode[A](seq: Seq[A], random: Random): A =
    seq(random.nextInt(seq.length))
}

