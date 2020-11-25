package Chord_Algo

import java.security.MessageDigest
import java.math.BigInteger

object Hash {
  // May adjust getHash to be more secure
  def getHash(k: String): String = {
    val md = MessageDigest.getInstance("SHA-1")
    val digest = md.digest(k.getBytes)
    val bigInt = new BigInteger(1,digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }
  def encrypt(hash: String, m: Int): Int = {
    val md = MessageDigest.getInstance("SHA-1")
    val key = md.digest(hash.getBytes("UTF-8"))
    val first4Bytes = key.slice(0,4)
    var hashCode = 0
    for (i <- first4Bytes.indices)
      hashCode = (hashCode << 8) + (first4Bytes(i)& 0xff)
    val mask = 0xffffffff >>> 16 - m
    hashCode = hashCode & mask
    hashCode
  }
  // movieTitles => m-bit hash value (unsigned)
  def m_bit_hash(movieTitles: String, m: Int): Int = {
    val md = java.security.MessageDigest.getInstance("SHA-1").digest(movieTitles.getBytes("UTF-8"))
    val m_bit_hash = Int(md, 16).toString(2).take(m)
    Integer.parseInt(m_bit_hash, 2)
  }
}
object test extends App{
  val david = Hash.getHash("David")
  //val encrypt = Hash.encrypt(david,)
  println(david)
  //println(encrypt)
}
