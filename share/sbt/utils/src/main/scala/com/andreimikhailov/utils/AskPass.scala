package com.andreimikhailov.utils

import javafx.scene.input.KeyCode

import akka.actor.ActorRef

import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.Platform
import scalafx.scene.control.PasswordField
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.VBox
import scalafx.stage.{Window, WindowEvent}


/** A dialog for password entry
  * 
  * @param message
  * @param actr  the actor to whom the entered password will be sent wrapped in [[PasswordPromptClosed]]
  */
class AskPass(mainwin: PrimaryStage, message: String, actr: ActorRef) {
  //this is very important; after we close the window, the FX thread should continue running,
  //so we can open the window again later:
  Platform.implicitExit = false

  val pwdField: PasswordField = new PasswordField {
    onKeyPressed = (event: KeyEvent) => if (event.getCode() == KeyCode.ENTER) {
      actr ! PasswordPromptClosed(Some(pwdField.text()))
      val st: Window = pwdField.getScene().getWindow()
      st.hide()
    } 
    prefWidth = 700
  }

  def start() = Platform.runLater{
    mainwin.setTitle(message)
    mainwin.scene.value.content = new VBox{
      children = Seq(pwdField)
    }
    mainwin.hide()
    mainwin.show()
    pwdField.requestFocus()
    mainwin.onCloseRequest = (we: WindowEvent) => {
      actr ! PasswordPromptClosed(None)
    }
  }

  /**
    * the method to bring up again the [[mainwin]] after it has been closed
    */
  def bringUp() = Platform.runLater {
    pwdField.text = ""
    mainwin.show()
  }

}


