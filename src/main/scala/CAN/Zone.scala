package CAN

import Chord_Algo.Hash
import com.typesafe.config.Config

object Zone{
  def apply(movieTitle: String): Zone = new Zone(movieTitle, Config)
}

class Zone(movieTitle: String, config: Config) {
  val m: Int = config.getInt("matrix_size")
  var key_space: Array[Array[Int]] = Array.ofDim(m, m)
  val n: Int = movieTitle.length
  val first_half: String = movieTitle.substring(0, n/2)
  val second_half: String = movieTitle.substring(n/2)
  var X: Double = Hash.encrypt(first_half, 8) % m
  var Y: Double = Hash.encrypt(second_half, 8) % m
  def getX: Double = this.X
  def getY: Double = this.Y
}
