package CAN

import CAN.Zone.neighborTable
import CAN.direction.{direction, left, up, right, down}
import Chord_Algo.Hash
import akka.actor.typed.ActorRef
import com.typesafe.config.ConfigFactory

object Zone extends direction {
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

  def splitZone(new_node: ActorRef[Node.Command]): Unit = {

  }
  def set_neighbor(node: ActorRef[Node.Command], zone: Zone): Unit = {
    val X_axis = zone.get_XRange
    val Y_axis = zone.get_YRange
    val x = this.get_XRange
    val y = this.get_YRange
    var direction: direction = left
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

object direction extends Enumeration {
  type direction = Value
  val left, up, right, down = Value
}