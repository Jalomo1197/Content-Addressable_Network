package CAN

import Chord_Algo.Hash
import com.typesafe.config.{Config, ConfigFactory}

object Zone{
  val m: Int = ConfigFactory.load("application.conf").getInt("matrix_size")

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
}
