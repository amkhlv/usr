package controllers

/**
 * Created by andrei on 11/03/15.
 */


import controllers.Utils.sha256

import org.mindrot.jbcrypt.BCrypt
import play.Logger
import play.api.Play

import scala.language.postfixOps

object Chat {

  val workdir: String = Play.current.configuration.getString("chat.workdir") match {
    case None => throw new Exception("conf parameter chat.workdir is missing from the file application.conf!")
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
        Logger.info("User " + user + " logged in")
      }
      matched
    }
  }

  def color(user: String) : String = {
    val userEntries: xml.NodeSeq = (specs \\ "user") filter (x => ((x \ "@name").text == user))
    (userEntries(0) \ "@color").text
  }

}

