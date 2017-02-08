package com.andreimikhailov.utils

import javafx.scene.input.KeyCode

import akka.actor.ActorRef

import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.PasswordField
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.VBox
import scalafx.stage.{Window, WindowEvent}
/**
  * Created by andrei on 2/7/17.
  */
class AskPass(message: String, actr: ActorRef) extends JFXApp {
  Platform.implicitExit = false
  val password: PasswordField = new PasswordField {
    onKeyPressed = (event: KeyEvent) => if (event.getCode() == KeyCode.ENTER) {
      actr ! PasswordPromptClosed(
        Some(password.text()),
        () => Platform.runLater {
          password.text = ""
          dialogStage.show()
        })
      val st: Window = password.getScene().getWindow()
      st.hide()
    }
  }
  password.setPrefWidth(300)
  val dialogStage: PrimaryStage = new PrimaryStage {
    title.value = message
    scene = new Scene {
      content = new VBox {
        children = Seq(password)
      }
    }
    password.requestFocus()
  }
  dialogStage.onCloseRequest = (we: WindowEvent) => {
    actr ! PasswordPromptClosed(
      None,
      () => Platform.runLater {
        password.text = ""
        dialogStage.show()
      })
  }
}


