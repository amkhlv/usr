package controllers

/**
  * Created by andrei on 1/7/17.
  */
import javax.inject._

import play.Configuration
import play.api.i18n.{I18nSupport, MessagesApi}
import play.filters.csrf.{CSRFAddToken, CSRFCheck}
import com.andreimikhailov.utils._
import play.api.mvc._

import collection.JavaConverters._
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow

import scala.xml.{Node, NodeSeq}


@Singleton
class PassController @Inject()(val addToken: CSRFAddToken,
                               val checkToken: CSRFCheck,
                               val messagesApi: MessagesApi,
                               val config: Configuration
                              ) extends Controller with I18nSupport {
  lazy val secrets = {
    val decryptor = new DecryptPGP(config.getString("application.passwords"))
    decryptor.result
  }

  def automate = addToken {
    Action { implicit  request =>
      val x = xml.XML.loadString(secrets)
      Ok(views.html.automatic((x \\ "site").map {s => new MyPassItem(s)}.toList.filter(pi => pi.hasSelenium)))
    }
  }
  def doSelenium(site: String, login: String) = addToken {
    Action { implicit request =>
      println("SITE->" + site + "<-")
      println("LOGIN->" + login + "<-")
      val (a,s): (Node,Node) = {
        val sites : NodeSeq = xml.XML.loadString(secrets).\\("site").filter(s => s.\@("nick").toString == site)
        val accounts = sites(0).\("account").filter(acc => acc.\@("login").toString == login)
        (accounts(0), sites(0).\("selenium")(0))
      }
      val robot = new MySelenium(a,s)
      robot.go
      Redirect("/automate")
    }
  }
}
