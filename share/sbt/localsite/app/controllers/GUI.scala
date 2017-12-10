package controllers

import scala.concurrent.Future

trait GUI {
  def checkMainWin(): Unit
  def warn(x: String): Unit
  def askForApproval(text: String) : Future[Boolean]
  def askPassphrase() : Future[String]
}

