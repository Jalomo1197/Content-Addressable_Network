package Chord


class FingerEntry(start: BigInt, interval: Interval, var node: Node) {
  def setNode(node: Node): Unit =
    this.node = node
  def getNode: Node =
    this.node
  def getStart: BigInt =
    this.start
  def getHash: BigInt =
    node.n
  def getInterval: Interval =
    this.interval
}
