package Chord


class FingerEntry(start: Int, interval: Interval, var node: Node) {
  def setNode(node: Node): Unit =
    this.node = node
  def getNode: Node =
    this.node
  def getStart: Int =
    this.start
  def getHash: Int =
    node.n
  def getInterval: Interval =
    this.interval
}
