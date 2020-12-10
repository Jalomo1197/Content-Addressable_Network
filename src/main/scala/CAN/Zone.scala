package CAN

import CAN.Zone.{left, right, up, down}
import Chord_Algo.Hash
import akka.actor.typed.ActorRef
import com.typesafe.config.ConfigFactory

object Zone extends Enumeration {
  type direction = Value
  val left, up, right, down = Value

  val m: Int = ConfigFactory.load("application.conf").getInt("matrix_size")
  // 0 -> x
  // 1 -> y


  def apply(X_range: (Double , Double), Y_range: (Double , Double)): Zone = new Zone(X_range, Y_range)

  def findLocation(movieTitle: String): (Double, Double) = {
    val n: Int = movieTitle.length
    val first_half: String = movieTitle.substring(0, n/2)
    val second_half: String = movieTitle.substring(n/2)
    val X: Double = Hash.encrypt(first_half, 8) % m
    val Y: Double = Hash.encrypt(second_half, 8) % m
    (X,Y)
  }

}



  class Zone(X_range: (Double, Double), Y_range: (Double, Double)) {
    // Ordering: Split -> x then y
    var split = 'x'
    var neighborTable: Neighbors = Neighbors()

    def setNeighborTable(index: Int, entry: Neighbor): Unit =
      neighborTable.neighbors(index) = entry
    def get_XRange: (Double, Double) = X_range

    def get_YRange: (Double, Double) = Y_range

    def containsP(P: (Double, Double)): Boolean = {
      val x_range = get_XRange
      val y_range = get_YRange
      P._1 >= x_range._1 && P._1 <= x_range._2 && P._2 >= y_range._1 && P._2 <= y_range._2
    }

    def closestPointToP(P: (Double, Double)): (Double, Double) = {
      P
    }

    def topLeft: (Double, Double) =
      (get_XRange._1, get_YRange._2)

    def topRight: (Double, Double) =
      (get_XRange._2, get_YRange._2)

    def botLeft: (Double, Double) =
      (get_XRange._1, get_YRange._1)

    def botRight: (Double, Double) =
      (get_XRange._2, get_YRange._1)

    // NeighborTable of Point P
    def identifyNeighborsToPointP(P: (Double, Double)): Neighbors = {
      // Identify Zone
      neighborTable
      // NeighborTable
    }

    def distance(P: (Double, Double), Q: (Double, Double)): Double =
      (P._2 - Q._2) / (P._1 - Q._1)

    def splitZone(new_node: ActorRef[Node.Command]): Unit = {
      // Add zone to Procedure
      //val zone: Unit = new_node ! Procedure[Node.Command]().getZone.get
      var start = 0.0
      var end = 0.0
      var new_node_half = (0.0, 0.0)
      var second_half = (0.0, 0.0)
      var new_node_zone = Zone(new_node_half, second_half)
      var current_node_zone = Zone(new_node_half, second_half)
      if (split == 'x') {
        start = get_XRange._1
        end = get_XRange._2
        //Divide Point P zone into two equal parts
        new_node_half = (start, start + (end - start))
        second_half = (start + (end - start), end)
        // Set Zone for new Node
        new_node_zone = Zone(new_node_half, get_YRange)
        current_node_zone = Zone(second_half, get_YRange)
        set_neighbor(new_node, current_node_zone)
        // Split Ordering for next split
        split = 'y'
      }
      else {
        start = get_YRange._1
        end = get_YRange._2
        //Divide Point P zone into two equal parts
        new_node_half = (start, start + (end - start))
        second_half = (start + (end - start), end)
        // Set Zone for new Node
        new_node_zone = Zone(get_XRange, new_node_half)
        current_node_zone = Zone(get_XRange, second_half)
        // Split Ordering for next split
        split = 'x'
      }
      // Update Neighbors for new node
      new_node_zone.setNeighborTable(0, neighborTable.neighbors(0))
      new_node_zone.setNeighborTable(1, neighborTable.neighbors(1))
      // Joining Node (right neighbor) is current occupant
      set_neighbor(new_node, current_node_zone)
      new_node_zone.setNeighborTable(3, neighborTable.neighbors(3))
      // Update (Left neighbor) of original occupant to new node
      // How do I get occupant ActorRef?
    }

    def set_neighbor(node: ActorRef[Node.Command], zone: Zone): Unit = {
      val X_axis = zone.get_XRange
      val Y_axis = zone.get_YRange
      val x = get_XRange
      val y = get_YRange
      var direction = left
      var entry = Neighbor(node, (0, 0), direction)
      // Same Zone
      if (x == X_axis && y == Y_axis) return
      /* Find Direction of neighbor */
      if (X_axis._1 < x._1 && X_axis._2 == x._1 && Y_axis == y)
        direction = left
      else if (X_axis._1 == x._2 && X_axis._2 > x._1 && Y_axis == y)
        direction = right
      else if (X_axis == x && Y_axis._1 == y._2 && Y_axis._2 > y._2)
        direction = up
      else if (X_axis == x && Y_axis._1 > y._2 && Y_axis._2 == y._2)
        direction = down
      // Set Up Entry
      if (direction == up || direction == down) {
        entry = Neighbor(node, X_axis, direction)
        if (direction == up) neighborTable.neighbors(1) = entry
        else neighborTable.neighbors(3) = entry
      }
      if (direction == left || direction == right) {
        entry = Neighbor(node, Y_axis, direction)
        if (direction == left) neighborTable.neighbors(0) = entry
        else neighborTable.neighbors(2) = entry
      }
    }
  }

