package CAN

import direction.{up, left, right, down}

object Neighbors{
  def apply(): Neighbors = new Neighbors()
}
class Neighbors {
  var neighbors: Array[Neighbor] = newNeighborTable()

  def newNeighborTable(): Array[Neighbor] = {
    val neighborTable: Array[Neighbor] = new Array[Neighbor](4)
    for (k <- 0 until 4)
      neighborTable(k) = new Neighbor(null, (0,0), up)
    neighborTable
  }

}
