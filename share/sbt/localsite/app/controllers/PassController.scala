package controllers

/**
  * Created by andrei on 1/7/17.
  */
import javax.inject._

import com.andreimikhailov.utils._
import org.openqa.selenium.chrome.ChromeOptions
import org.openqa.selenium.firefox.FirefoxProfile
import org.openqa.selenium.firefox.internal.ProfilesIni
import play.Configuration
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._
import play.filters.csrf.{CSRFAddToken, CSRFCheck}

import scala.concurrent.ExecutionContext
import scala.xml.{Node, NodeSeq}


@Singleton
class PassController @Inject()(val addToken: CSRFAddToken,
                               val checkToken: CSRFCheck,
                               val messagesApi: MessagesApi,
                               val config: Configuration
                              ) extends Controller with I18nSupport {
  implicit val ec = ExecutionContext.global

  lazy val secrets = {
    val decryptor = new Decryptor(Common.mainwin, config.getString("application.passwords"))
    decryptor.result
  }
  case class NeitherFirefoxProfileNorChromeProfileSpecified() extends  Exception
  val profile: ProfilesIni= new ProfilesIni()
  val myprofile: Either[FirefoxProfile,ChromeOptions] =
    if (config.keys.contains("application.firefoxProfile")) {
      Left(profile.getProfile(config.getString("application.firefoxProfile")))
    } else if (config.keys.contains("application.chromeProfile")) {
      val chromeOptions = new ChromeOptions()
      chromeOptions.addArguments("user-data-dir=" + config.getString("application.chromeProfile"))
      Right(chromeOptions)
    } else {
      throw NeitherFirefoxProfileNorChromeProfileSpecified()
    }
  def automate =
    Action { implicit  request =>
      val x = xml.XML.loadString(secrets)
      Ok(views.html.automatic((x \\ "site").map {s => new MyPassItem(s)}.toList.filter(pi => pi.hasSelenium)))
    }
  def tooltips =
    Action { implicit request =>
      val x = xml.XML.loadString(secrets)
      Ok(views.html.tooltips((x \\ "site").map {s => new MyPassItem(s)}.toList.filter(pi => pi.canShow)))
    }
  def doSelenium(site: String, login: String) =
    Action { implicit request =>
      println("SITE->" + site + "<-")
      println("LOGIN->" + login + "<-")
      val (a,s): (Node,Node) = {
        val sites : NodeSeq = xml.XML.loadString(secrets).\\("site").filter(s => s.\@("nick").toString == site)
        val accounts = sites(0).\("account").filter(acc => acc.\@("login").toString == login)
        (accounts(0), sites(0).\("selenium")(0))
      }
      val robot = new MySelenium(a,s,myprofile)
      robot.go
      Redirect("/automate")
    }
}
