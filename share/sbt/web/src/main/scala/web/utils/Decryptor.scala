package web.utils

import java.io.ByteArrayOutputStream
import scala.sys.process._

import web.conf

object Decryptor {
  val filename = conf().getString("secretFile")
  val cmd = Seq("gpg", "--decrypt", filename)
  def decrypt = {
    val output = new ByteArrayOutputStream()
    val r = (cmd #> output).!
    val decrypted: Option[String] = if (r > 0) None else Some(output.toString("UTF-8"))
    output.close()
    decrypted match {
      case Some(x) => x
      case None =>
        println(s"=== unable do decrypt $filename ===")
        throw new Error(s"unable to decrypt $filename")
    }
  }
}
