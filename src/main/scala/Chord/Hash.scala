package Chord

import java.security.MessageDigest
import java.math.BigInteger

object Hash {
  def getHash(k: String): String = {
    val md = MessageDigest.getInstance("SHA-1")
    val digest = md.digest(k.getBytes)
    val bigInt = new BigInteger(1,digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }
  def encrypt(hash: String): BigInt ={
    val md = MessageDigest.getInstance("SHA-1")
    val key = BigInt(md.digest(hash.getBytes("UTF-8")).map("%02x".format(_)).mkString,16)
    key
  }
}
object test extends App{
  val david = Hash.getHash("David")
  val encrypt = Hash.encrypt(david)
  println(david)
  println(encrypt)
}
