package controllers

import actors.UserActor
import akka.actor.ActorRef
import play.api.Logger
import play.api.Play.current
import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.JsValue
import play.api.mvc.{Action, Controller, WebSocket}
import play.filters.csrf._


import scala.concurrent.Future

object Application extends Controller {
  //val UID = "uid"
  val UID = "username"
  var counter = 0;
  var toks : Set[String] = Set();
  var origins : Map[String,String] = Map();
  var trustedActors : Set[ActorRef] = Set();
  def isSecure:Boolean =
    Play.current.configuration.getString("chat.secure") match {
      case Some(s) => s.toBoolean
      case None => throw new RuntimeException("ERROR: have to define chat.secure in Configuration")
    }
  def location:String = Play.current.configuration.getString("chat.origin") match {
      case Some(s) => s.toString
      case None => throw new RuntimeException("ERROR: have to define chat.location in Configuration")
    }
  val loginForm: Form[Login] = Form(
    mapping(
      "login" -> text,
      "password" -> text
    )(Login.apply)(Login.unapply)
  )
  def initLogin = Action {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      Ok(views.html.login(
        "Please log in",
        loginForm
      ))
    }
  }
  def login = Action {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      if (loginForm.hasErrors) {
        Logger.error("Bad Request: " + loginForm.errors.toString())
        BadRequest("Bad Request: " + loginForm.errors.toString())
      } else {
        val loginData: Login = loginForm.bindFromRequest().get
        //println(request.headers)
        if (Chat.passwordOK(loginData.email, loginData.password)) {
          toks = toks + token.value
          origins = origins + (loginData.email -> request.headers.get("Origin").getOrElse("http://localhost:9000"))
          // this is because browsers do not send Origin header  when Origin is localhost ^^^^^^^^^^^^^^^^^^^^^^^
            Ok(
              views.html.index(loginData.email, token.value)
            ).withSession {
            request.session +("username", loginData.email)
          }
        }
        else Ok(views.html.login(
          "Password was incorrect, please try again",
          loginForm
        ))
      }
    }
  }
  def index = Action {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      request.session.get("username") match {
        case None => Ok(views.html.login(
          "Please log in",
          loginForm
        ))
        case Some(uname) => {
          Logger.info("User " + uname + " requested INDEX ")
          //toks = toks + (uname -> token.value)
          Ok(views.html.index(uname, "NO_TOKEN"))
        }
      }
    }
  }
  def ws = WebSocket.tryAcceptWithActor[JsValue, JsValue] { implicit request =>
    Future.successful(request.session.get(UID) match {
      case None => Left(Forbidden)
      case Some(uid) => {
        val origin: String = request.headers.get("Origin").getOrElse("MISSING_ORIGIN")
        if (origin == location) Right(UserActor.props(uid)) else {
          Logger.warn(" === Origin mismatch: " + origin + " vs " + origins(uid) + " === ")
          Left(Forbidden)
        }
      }
    })
  }
}

case class Login (email: String, password: String)
