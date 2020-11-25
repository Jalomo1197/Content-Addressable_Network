package Chord_Algo

import akka.actor.typed.ActorRef

object FingerEntry{
  def apply(start: Int, interval: Interval, node: ActorRef[Node.Command]): FingerEntry = new FingerEntry(start, interval, node)
}
class FingerEntry(start: Int, var interval: Interval, var node: ActorRef[Node.Command]) {
  def setNode(node: ActorRef[Node.Command]): Unit =
    this.node = node
  def getNode: ActorRef[Node.Command] =
    this.node
  def getStart: Int =
    this.start
  def getInterval: Interval =
    this.interval
  def setInterval(interval: Interval): Unit =
    this.interval = interval
  def printInterval(): String =
    "[" + start + ", " + this.getInterval.get_end + ")"
}