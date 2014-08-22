package controllers


import java.io.{File, FileReader}
import java.security.MessageDigest
import org.mindrot.jbcrypt.BCrypt

import scala.collection.JavaConverters._
import org.markdown4j.Markdown4jProcessor
import scala.language.postfixOps
import play.api.Play
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator
import Utils.sha256
/**
 * Created by andrei on 24/07/14.
 */

object Pile {

  val logger = Logger.getLogger("PileUtils")
  PropertyConfigurator.configure(
    Play.current.configuration.getString("application.logconf") match {
      case None => throw new Exception("conf parameter application.logconf is missing from the file application.conf!")
      case Some(s) => s
    }
  )
  val workdir: String = Play.current.configuration.getString("application.workdir") match {
    case None => throw new Exception("conf parameter application.workdir is missing from the file application.conf!")
    case Some(s) => s
  }
  val specs = xml.XML.loadFile(workdir + "/config.xml")

  def passwordOK(user: String, password: String): Boolean = {
    if (Play.current.configuration.getString("application.secret") == Some("TESTING")) {
      println("ERROR: === I am in testing mode ! ===")
      false
    } else {
      val userEntries: xml.NodeSeq = (specs \\ "user") filter (x => ((x \ "@name").text == user))
      val matched: Boolean = (false /: userEntries)(
        (b: Boolean, uentry: xml.Node) => b || BCrypt.checkpw(sha256(password), (uentry \ "@hash").text)
          //(sha256((uentry \ "@salt").text + password) == (uentry \ "@hash").text)
      )
      if (matched) {
        logger.info("User " + user + " logged in")
      }
      matched
    }
  }
  def filesInUserDir(user: String) : List[File] = {
    val dir = new File(workdir + "/" + user)
    dir.listFiles().toList
  }

  def markdown(user: String) : String = {
    val processor = new Markdown4jProcessor()
    processor.process(new File(workdir + "/" + user + ".md"))
  }

}
