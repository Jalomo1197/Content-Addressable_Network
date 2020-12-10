package CAN

import Zone.up

object Neighbors{
  def apply(): Neighbors = new Neighbors()
}
class Neighbors {
  /*
  *   Entry | Direction
  *     0   | Left
  *     1   | Up
  *     2   | Right
  *     3   | Down
  *
  */
  var neighbors: Array[Neighbor] = newNeighborTable()

  def newNeighborTable(): Array[Neighbor] = {
    val neighborTable: Array[Neighbor] = new Array[Neighbor](4)
    for (k <- 0 until 4)
      neighborTable(k) = new Neighbor(null, (0,0), up)
    neighborTable
  }

}
