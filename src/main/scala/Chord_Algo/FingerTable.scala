package Chord_Algo

import akka.actor.typed.ActorRef

import scala.math.pow


object FingerTable{
  def apply(hashKey: Int, m: Int): FingerTable = new FingerTable(hashKey, m)
}

class FingerTable(n: Int, m: Int) {
  var finger: Array[FingerEntry] = newFingerTable()



  /*
       Page 4:: " 4.3 Scalable Key Location "
       https://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf
       finger[k].start = ( n + 2^(k-1) ) mod 2^m
  */
  def newFingerTable(): Array[FingerEntry] = {
    val fingerTable: Array[FingerEntry] = new Array[FingerEntry](m + 1)
    for (k <- 1 to m){
      val start =  calculateStart(k)
      fingerTable(k) = new FingerEntry(start, Interval(start, calculateStart(k + 1)), null)
    }
    fingerTable
  }

  def calculateStart(index: Int): Int = ( n + scala.math.pow(2, index - 1).toInt ) % pow(2, m).toInt


  def initializeNodeColumn(n : ActorRef[Node.Command]): Unit = {                                           // new node perspective
    for (i <- 1 to m) {
      this.finger(i).setNode(n)
    }
  }
}
