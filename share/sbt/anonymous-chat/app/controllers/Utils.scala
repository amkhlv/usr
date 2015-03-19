package controllers

/**
 * Created by andrei on 11/03/15.
 */

import org.mindrot.jbcrypt.BCrypt
import scala.language.postfixOps


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
  def mksecret = {
    val r = new java.security.SecureRandom
    println((1 to 64).map(_=>(r.nextInt(74)+48).toChar).mkString.replaceAll("\\\\+", "/"))
  }
}

