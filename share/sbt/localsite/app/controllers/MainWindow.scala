package controllers

import java.util.concurrent.CountDownLatch

import akka.actor.Actor

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.{BooleanProperty, StringProperty}
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color._

class MainWindow(dispatcher: MainWindowDispatcher) extends JFXApp {
  Platform.implicitExit = false
  stage = new JFXApp.PrimaryStage {
    title.value = "You Are Welcome !"
    width = 600
    height = 450
    scene = new Scene { fill = LightCoral   }
  }
  def wakeUp = Platform.runLater {
    stage.scene.value.fillProperty().setValue(LightGreen)
  }
}

case class PasswordIs(password: String)

class MainWindowDispatcher extends Actor {
  lazy val mainwin = new MainWindow(this)
  def checkwin = {
    try {
      mainwin.wakeUp
    } catch {
      case error: java.lang.IllegalStateException => {
        println("Illegal State Exception")
        Future {
          blocking {
            mainwin.main(List().toArray[String])
          }
        }
      }
      case x: Exception => println("=== Strange Exception ===")
    }
  }
  def receive = {
    case CheckMainWin => checkwin
    case ApprovalRequest(question:String, token:String) => {
      checkwin
      val boolProp = new BooleanProperty()
      val stringProp = new StringProperty()
      val latch = new CountDownLatch(1)
      Platform.runLater {
        val ButtonTypeDeny = new ButtonType("Deny")
        val ButtonTypeAllow = new ButtonType("Allow")
        val alert = new Alert(AlertType.Confirmation) {
          initOwner(mainwin.stage)
          title = "Localsite Approval Request"
          headerText = question
          contentText = "Choose your option"
          buttonTypes = Seq(ButtonTypeDeny, ButtonTypeAllow)
        }
        val result = alert.showAndWait()
        result match {
          case Some(ButtonTypeDeny) => {
            println("-- user denied")
            boolProp.setValue(false)
            stringProp.setValue(token)
            latch.countDown()
          }
          case Some(ButtonTypeAllow) => {
            boolProp.setValue(true)
            stringProp.setValue(token)
            latch.countDown()
          }
          case _ => {
            println("-- user chose CANCEL or closed the dialog")
            boolProp.setValue(false)
            stringProp.setValue("NODIALOG")
            latch.countDown()
          }
        }
      }
      latch.await()
      sender() ! ApprovalResponse(boolProp.get(), stringProp.get())
    }
    case AskPass() => {
      checkwin
      val stringProp = new StringProperty()
      val latch = new CountDownLatch(1)
      Platform.runLater{
        val dialog = new Dialog[PasswordIs]() {
          initOwner(mainwin.stage)
          title = "Passphrase Entry Dialog"
          headerText = "Please enter your passphrase"
        }
        val loginButtonType = new ButtonType("enter", ButtonData.OKDone)
        val bTypes = dialog.dialogPane().getButtonTypes()
        bTypes.addAll(loginButtonType, ButtonType.Cancel)
        val password = new PasswordField() {
          promptText = "passphrase"
        }
        val grid = new GridPane() {
          hgap = 10
          vgap = 10
          padding = Insets(20, 100, 10, 10)
          add(new Label("Passphrase:"), 0, 1)
          add(password, 1, 1)
        }
        dialog.dialogPane().setContent(grid)
        password.requestFocus()
        dialog.resultConverter = dialogButton =>
          if (dialogButton == loginButtonType)
            PasswordIs(password.text())
          else
            null
        val result = dialog.showAndWait()
        result match {
          case Some(PasswordIs(p)) => {
            stringProp.setValue(p)
            latch.countDown()
          }
          case None               => {
            stringProp.setValue("")
            latch.countDown()
          }
        }
      }
      latch.await()
      sender() ! stringProp.get()
    }
    case ShowWarning(txt) => {
      checkwin
      Platform.runLater {
        new Alert(AlertType.Warning) {
          initOwner(mainwin.stage)
          title = "Localsite Warning"
          headerText = txt
        }.showAndWait()
      }
    }
  }
}
