package Chord

import java.security.MessageDigest


class FingerEntry(start: Int, interval: Interval, var node: Node) {
  def setNode(node: Node): Unit =
    this.node = node
  def getNode: Node =
    this.node
  def getStart: Int =
    this.start
  def getHash: String =
    node.hash
  def decrypt(hash: String): BigInt ={
    val md = MessageDigest.getInstance("SHA-256")
    val key = BigInt(md.digest(hash.getBytes("UTF-8")).map("%02x".format(_)).mkString,16)
    key
  }
  def getInterval: Interval =
    this.interval
}
