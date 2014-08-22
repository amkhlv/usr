package controllers

import org.mindrot.jbcrypt.BCrypt
import scala.language.postfixOps

/**
 * Created by andrei on 21/08/14.
 */
object Utils {
  def sha256(s: String) = {
    val m = java.security.MessageDigest.getInstance("SHA-256")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    new java.math.BigInteger(1, m.digest()).toString(16)
  }
  def mkpasswd = {
    val plaintext = readLine("plaintext: ")
    val x: String = sha256(plaintext)
    println("------------->  ")
    println(BCrypt.hashpw(x, BCrypt.gensalt()))
  }
  def stringToHexString(s: String) : String = {
    val cs = s.toCharArray toList
    val is = cs map ( c => c.toInt )
    (cs map ((ch : Char) => ch.toInt.toHexString )) mkString("_")
  }
  def hexStringToString(h: String) : String = {
    val hs = h.split("_")
    (hs map ((x: String) => Integer.parseInt(x,16).toChar)) mkString
  }
}
