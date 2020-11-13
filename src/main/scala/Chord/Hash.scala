package Chord

import java.security.MessageDigest
import java.math.BigInteger

object Hash {
  def getHash(k: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val digest = md.digest(k.getBytes)
    val bigInt = new BigInteger(1,digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }
}
object test extends App{
  println(Hash.getHash("David"))
  println(Hash.getHash("Jacob"))
  println(Hash.getHash("Jacob"))
}
