package CAN

import CAN.Zone.{Down, Left, Right, Up, default}
import akka.actor.typed.ActorRef
import com.typesafe.config.ConfigFactory

object Zone extends Enumeration {
  type direction = Value
  val Left, Up, Right, Down, default = Value

  val m: Int = ConfigFactory.load("application.conf").getInt("matrix_size")

  def apply(X_range: (Double , Double), Y_range: (Double , Double)): Zone = new Zone(X_range, Y_range)

  def findLocation(movieTitle: String): (Double, Double) = {
// encrypt using SHA-1 Format
    val md = java.security.MessageDigest.getInstance("SHA-1")
    println(md.digest(movieTitle.getBytes("UTF-8")).map("%02x".format(_)).mkString)

    // take the first four after SHA1 conversion
    val firstFour = md.digest(movieTitle.getBytes("UTF-8")).map("%02x".format(_)).mkString.slice(0,4).map(_.toUpper)
    //println(firstFour)

    // convert SHA-1 to coordinate plane of 16x16 for (x,y)
    // X,Y = Hash(Sum(hash(0,2)), Hash(Sum(hash(2,4))
    var x =
    (firstFour.slice(0,1).
      replace("A","10").replace("B","11").replace("C","12").
      replace("D","14").replace("E","15").replace("F","16").toDouble
      +
      firstFour.slice(1,2).
        replace("A","10").replace("B","11").replace("C","12").
        replace("D","14").replace("E","15").replace("F","16").toDouble)/2.0

    var y =
      (firstFour.slice(2, 3).
        replace("A", "10").replace("B", "11").replace("C", "12").
        replace("D", "14").replace("E", "15").replace("F", "16").toDouble
        +
        firstFour.slice(3, 4).
          replace("A", "10").replace("B", "11").replace("C", "12").
          replace("D", "14").replace("E", "15").replace("F", "16").toDouble) / 2.0

    // if x or y lay on the border, we always go x+1, or y+1 to designate it into proper zone
    if(y == 7.0){ y += 1 }
    if(x == 7.0){ x += 1 }
    (x,y)
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

    def formatZone: String = s"X Range: $X_range Y Range: $Y_range"

    def containsP(P: (Double, Double)): Boolean =
      P._1 >= get_XRange._1 && P._1 <= get_XRange._2 && P._2 >= get_YRange._1 && P._2 <= get_YRange._2

    def closestPointToP(procedure: Procedure[Node.Command]): List[ActorRef[Node.Command]] = {
      // Assume P not within zone
      val P: (Double, Double) = procedure.getLocation.get
      // Scan x-axis, If P's X value is within range
      if(closestX(P))
        closestPointViaY(P) // up or down
      // Scan y - axis, If P's Y value is within range
      if(closestY(P)) closestPointViaX(P) // left or right
      // Vertex
      if(P._1 > get_XRange._2 && P._2 > get_YRange._2) return topRight
      if(P._1 > get_XRange._2 && P._2 < get_YRange._1) return botRight
      if(P._1 < get_XRange._1 && P._2 < get_YRange._1) botLeft else topLeft
      // Check closest has not been visited
    }


    def closestX(P: (Double, Double)): Boolean =
      P._1 > get_XRange._1 && P._1 < get_XRange._2

    def closestPointViaY(P: (Double, Double)): (Double, Double) = {
      // up else down
      if(P._2 > get_YRange._2) (P._1, get_YRange._2) else (P._1, get_YRange._1)
    }

    def closestPointViaX(P: (Double, Double)): (Double, Double) = {
      // right else left
      if(P._1 > get_XRange._2) (get_XRange._2, P._2) else (get_XRange._1, P._2)
    }

    def closestY(P: (Double, Double)): Boolean =
      P._2 > get_YRange._1 && P._2 < get_YRange._2

    def topLeft: (Double, Double) =
      (get_XRange._1, get_YRange._2)

    def topRight: (Double, Double) =
      (get_XRange._2, get_YRange._2)

    def botLeft: (Double, Double) =
      (get_XRange._1, get_YRange._1)

    def botRight: (Double, Double) =
      (get_XRange._2, get_YRange._1)

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
      // Split in x direction
      if (split == 'x') {
        start = get_XRange._1
        end = get_XRange._2
        //Divide Point P zone into two equal parts
        new_node_half = (start, start + (end - start))
        second_half = (start + (end - start), end)
        // Set Zone for new Node
        new_node_zone = Zone(new_node_half, get_YRange)
        current_node_zone = Zone(second_half, get_YRange)
        // Update new_node right neighbor (original occupant)
        set_neighbor(new_node, current_node_zone)
        // Update original occupant left neighbor (new_node)
        // Split Ordering for next split
        split = 'y'
      }
      // Split in y direction
      else {
        start = get_YRange._1
        end = get_YRange._2
        //Divide Point P zone into two equal parts
        new_node_half = (start, start + (end - start))
        second_half = (start + (end - start), end)
        // Set Zone for new Node
        new_node_zone = Zone(get_XRange, new_node_half)
        current_node_zone = Zone(get_XRange, second_half)
        // Update new_node Top neighbor (original occupant)
        set_neighbor(new_node, current_node_zone)
        // Update original occupant left neighbor (new_node)

        // Split Ordering for next split
        split = 'x'
      }
      val t = neighborTable.neighbors(0)
      // Update Neighbors for new node
      new_node_zone.setNeighborTable(0, neighborTable.neighbors(0))
      new_node_zone.setNeighborTable(1, neighborTable.neighbors(1))
      // Joining Node (right neighbor) is current occupant
      set_neighbor(new_node, current_node_zone)
      new_node_zone.setNeighborTable(3, neighborTable.neighbors(3))
      // Update (Left neighbor) of original occupant to new node
      // How do I get occupant ActorRef?
    }
    def updateNeighbors(): Unit = {

    }
    def findDirection(zone: Zone): Zone.direction = {
      val X_axis = zone.get_XRange
      val Y_axis = zone.get_YRange
      if(X_axis._1 < get_XRange._1 && X_axis._2 == get_XRange._1 && Y_axis == get_YRange) Left
      else if(X_axis._1 == get_XRange._2 && X_axis._2 > get_XRange._1 && Y_axis == get_YRange) Right
      else if(X_axis == get_XRange && Y_axis._1 == get_YRange._2 && Y_axis._2 > get_YRange._2) Up
      else Down
    }

    def identicalZone(zone: Zone): Boolean =
      get_XRange == zone.get_XRange && get_YRange == zone.get_YRange

    def set_neighbor(node: ActorRef[Node.Command], zone: Zone): Unit = {
      val X_axis = zone.get_XRange
      val Y_axis = zone.get_YRange
      var direction = default
      var entry = Neighbor(node, (0, 0), direction)
      // Same Zone
      if (identicalZone(zone)) return
      /* Find Direction of neighbor */
      direction = findDirection(zone)
      // Set Up Entry
      if (direction == Up || direction == Down) {
        entry = Neighbor(node, X_axis, direction)
        if (direction == Up) neighborTable.neighbors(1) = entry
        else neighborTable.neighbors(3) = entry
      }
      if (direction == Left || direction == Right) {
        entry = Neighbor(node, Y_axis, direction)
        if (direction == Left) neighborTable.neighbors(0) = entry
        else neighborTable.neighbors(2) = entry
      }
    }
  }

