package CAN

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object Bootstrap{
  def apply():  Behavior[Command] = Behaviors.setup(context => new Bootstrap(context))

  trait Command

  final case object initializeZones extends Command
  case class getNodeInNetwork(p: Procedure[Node.Command]) extends Command
}

class Bootstrap(context: ActorContext[Bootstrap.Command]) extends AbstractBehavior[Bootstrap.Command](context){
  import Bootstrap._

  var zone_count = 0
  var active_nodes: List[Node.Command] = List.empty[Node.Command]
  // Assign Zones
  // Assign Neighbors

  override def onMessage(msg: Bootstrap.Command): Behavior[Bootstrap.Command] = {
  msg match {
    case initializeZones =>
      initializeNeighbors()
      this.zone_count += 4
      this
    case getNodeInNetwork(p) =>
      this
  }
  }
  def initializeNeighbors():Unit = {
    /* Spawn New Nodes */
    for(i <- 0 until 4){
      val new_node = context.spawn(, )
    }
    val zone = Zone((0, 7), (0, 7))
    val zone2 = Zone((7, 15), (0, 7))
    val zone3 = Zone((0, 7), (7, 15))
    val zone4 = Zone((7, 15), (7, 15))
    /* Zones must acknowledge each other as neighbors */
    zone.set_neighbors(List(zone2, zone3))
    zone2.set_neighbors(List(zone, zone4))
    zone3.set_neighbors(List(zone, zone4))
    zone4.set_neighbors(List(zone2, zone3))
  }


}

