package CAN

import CAN.Zone.{left, right, up, down, neighborTable}
import Chord_Algo.Hash
import akka.actor.typed.ActorRef
import com.typesafe.config.ConfigFactory

object Zone extends Enumeration {
  type direction = Value
  val left, up, right, down = Value

  val m: Int = ConfigFactory.load("application.conf").getInt("matrix_size")
  var neighborTable: Neighbors = Neighbors()

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

class Zone(X_range: (Double , Double), Y_range: (Double , Double)) {
  def get_XRange: (Double , Double) = X_range
  def get_YRange: (Double , Double) = Y_range

  def containsP(P: (Double, Double)): Boolean = {
    val x_range = get_XRange
    val y_range = get_YRange
    P._1 >= x_range._1 && P._1 <= x_range._2 && P._2 >= y_range._1 && P._2 <= y_range._2
  }
  def identifyNeighborToPointP(P: (Double, Double)): Neighbor = {
    // Four Corners
    val top_left: (Double, Double) = (get_XRange._1, get_YRange._2)
    val top_right: (Double, Double) = (get_XRange._2, get_YRange._2)
    val bot_left: (Double, Double) = (get_XRange._1, get_YRange._1)
    val bot_right: (Double, Double) = (get_XRange._2, get_YRange._1)
    // Four middle points on each zone border
    val left_mid: (Double, Double) = (top_left._1, (top_left._2 - bot_left._2)/2)
    val top_mid: (Double, Double) = ((top_right._1 - top_left._1)/2 ,top_left._2)
    val right_mid: (Double, Double) = (top_right._1, (top_right._2 - bot_right._2)/2)
    val bot_mid: (Double, Double) = ((bot_right._1 - bot_left._1)/2, bot_left._2)
    // Right Neighbor
    if(P._1 > top_mid._1 && 2*distance(P, right_mid) < (distance(P, top_right) + distance(P, bot_right))) neighborTable.neighbors(2)
    // Down Neighbor
    if(P._2 < left_mid._2 && 2*distance(P, bot_mid) < (distance(P, bot_left) + distance(P, bot_right))) neighborTable.neighbors(3)
    // Left Neighbor
    else if(P._1 < top_mid._1 && 2*distance(P, left_mid) < (distance(P, top_left) + distance(P, bot_left))) neighborTable.neighbors(0)
    // Up Neighbor
    else if(P._2 > left_mid._2 && 2*distance(P, top_mid) < (distance(P, top_left) + distance(P, top_right))) neighborTable.neighbors(1)
    // Edge Cases (Vertices)
    // P is completely in the middle (UP)
    else neighborTable.neighbors(1)
  }
  def distance(P: (Double, Double), Q: (Double, Double)): Double =
    (P._2 - Q._2)/(P._1 - Q._1)
  def splitZone(new_node: ActorRef[Node.Command]): Unit = {

  }
  def set_neighbor(node: ActorRef[Node.Command], zone: Zone): Unit = {
    val X_axis = zone.get_XRange
    val Y_axis = zone.get_YRange
    val x = this.get_XRange
    val y = this.get_YRange
    var direction = left
    var entry = Neighbor(node, (0, 0), direction)
    // Same Zone
    if(x == X_axis && y == Y_axis) return
    /* Find Direction of neighbor */
    if(X_axis._1 < x._1 && X_axis._2 == x._1 && Y_axis == y)
      direction = left
    else if(X_axis._1 ==  x._2 && X_axis._2 > x._1 && Y_axis == y)
      direction = right
    else if(X_axis == x && Y_axis._1 == y._2 && Y_axis._2 > y._2)
      direction = up
    else if(X_axis == x && Y_axis._1 > y._2 && Y_axis._2 == y._2)
      direction = down
    // Set Up Entry
    if(direction == up || direction == down) {
      entry = Neighbor(node, X_axis, direction)
      if(direction == up)  neighborTable.neighbors(1) = entry
      else neighborTable.neighbors(3) = entry
    }
    if(direction == left || direction == right) {
      entry =  Neighbor(node, Y_axis, direction)
      if(direction == left)  neighborTable.neighbors(0) = entry
      else neighborTable.neighbors(2) = entry
    }
  }
}
