package controllers

import play.api.Play
import play.api.data.Form
import play.api.mvc.{AnyContent, Action, Controller, Result}
import play.api.data.Forms._
import play.filters.csrf._
import java.nio.file.Paths
import java.io.File
import views.html.defaultpages.badRequest
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator

object Application extends Controller {
  val logger = Logger.getLogger("Application")
  PropertyConfigurator.configure(
    Play.current.configuration.getString("application.logconf") match {
      case None => throw new Exception("conf parameter application.logconf is missing from the file application.conf!")
      case Some(s) => s
    }
  )
  def warning(message: String) = Action {
    implicit request => {
      Ok(views.html.warning(message))
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
          logger.info("User " + uname + " requested INDEX ")
          Ok(views.html.index(
            uname,
            "Olá " + uname,
            Pile.filesInUserDir(uname) map (f => Utils.hexStringToString(f.getName))
          ))
        }
      }
    }
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
        logger.error("Bad Request: " + loginForm.errors.toString())
        BadRequest("Bad Request: " + loginForm.errors.toString())
      } else {
        val loginData: Login = loginForm.bindFromRequest().get
        if (Pile.passwordOK(loginData.email, loginData.password)) {
          Ok(views.html.index(
            loginData.email,
            "we are ready to store your files",
            Pile.filesInUserDir(loginData.email) map (f => Utils.hexStringToString(f.getName))
          )).withSession(request.session +("username", loginData.email))
        }
        else Ok(views.html.login(
          "Password was incorrect, please try again",
          loginForm
        ))
      }
    }
  }
  def upload = Action(parse.multipartFormData) {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      request.body.file("userdocument").map { uploadedDoc =>
        request.session.get("username") match {
          case None => Ok(views.html.login(
            "Please log in",
            loginForm
          ))
          case Some(uname) => {
            val humanReadableFileName = uploadedDoc.filename
            val filename = Utils.stringToHexString(humanReadableFileName)
            if (filename.length > 255) {
              Ok(views.html.index(
                uname,
                "ERROR: === File name is too long ===",
                Pile.filesInUserDir(uname) map (f => Utils.hexStringToString(f.getName))
              ))
            }
            else {
              val contentType = uploadedDoc.contentType
              val fl = new File(Pile.workdir + "/" + uname + "/" + filename)
              if (fl.exists()) {
                logger.info("User " + uname + " is about to replace the existing file " + humanReadableFileName)
                fl.delete()
              }
              uploadedDoc.ref.moveTo(fl)
              logger.info("User " + uname + " uploaded " + humanReadableFileName)
              Ok(views.html.index(
                uname,
                "File uploaded ",
                Pile.filesInUserDir(uname) map (f => Utils.hexStringToString(f.getName))
              ))
            }
          }
        }
      }.getOrElse {
        Redirect(routes.Application.index).flashing(
          "error" -> "Missing file"
        )
      }
    }
  }
  def download(fname: String) = Action {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      request.session.get("username") match {
        case None => Ok(views.html.login(
          "Please log in",
          loginForm
        ))
        case Some(uname) => {
          // logged in OK
          val hexname = Utils.stringToHexString(fname)
          if (hexname.length > 255) {
            logger.error("=== SECURITY === Hex filename too long when trying to download")
            Ok(views.html.index(
              uname,
              "ERROR: === File name is too long ===",
              Pile.filesInUserDir(uname) map (f => Utils.hexStringToString(f.getName))
            ))
          } else {
            logger.info("User " + uname + " downloaded " + fname)
            Ok.sendFile(
              new File(Pile.workdir + "/" + uname + "/" + hexname),
              fileName = _ => fname
            )
          }
        }
      }
    }
  }
  def deleteFile(fname: String) = Action {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      request.session.get("username") match {
        case None => Ok(views.html.login(
          "Please log in",
          loginForm
        ))
        case Some(uname) => {
          // logged in OK
          val hexname = Utils.stringToHexString(fname)
          if (hexname.length > 255) {
            logger.error("=== SECURITY === Hex filename too long when trying to delete")
            Ok(views.html.index(
              uname,
              "ERROR: === File name is too long ===",
              Pile.filesInUserDir(uname) map (f => Utils.hexStringToString(f.getName))
            ))
          } else {
            // OK let us delete
            val fl = new File(Pile.workdir + "/" + uname + "/" + Utils.stringToHexString(fname))
            if (fl.exists()) {
              fl.delete()
            }
            logger.info("User " + uname + " deleted file " + fname)
            Ok(views.html.index(
              uname,
              "=== file " + fname + " deleted ===",
              Pile.filesInUserDir(uname) map (f => Utils.hexStringToString(f.getName))
            ))
          }
        }
      }
    }
  }
  def logoutConfirmation = Action {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      request.session.get("username") match {
        case None => {
          logger.error("Logout of the user which is not logged in ???")
          Ok(views.html.login(
            "Please log in",
            loginForm
          ))
        }
        case Some(uname) => {
          logger.info("User " + uname + " is about to confirm if they want to logout ")
          Ok(views.html.logout(uname))
        }
      }
    }
  }
  /**
   * Logout and clean the session.
   */
  def logout = Action {
    implicit request => {
      implicit val token = CSRF.getToken(request).get
      logger.info("User logged out")
      Redirect(
        "https://log:out@" + Play.current.configuration.getString("application.url").get
      ).withNewSession.flashing(
          "success" -> "You've been logged out"
        )
    }
  }
}

case class Login (email: String, password: String)

