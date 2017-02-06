package com.andreimikhailov.utils

/**
  * Created by andrei on 1/6/17.
  */

import javafx.embed.swing.JFXPanel
import javafx.event.EventHandler
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.stage.WindowEvent

import akka.actor.ActorRef

import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.control.PasswordField
import scalafx.stage.Stage

class AskPass(message: String, actr: ActorRef) {
  //  https://github.com/scalafx/ScalaFX-Tutorials/blob/master/stand-alone-dialog/src/main/scala/stand_alone_dialog/StandAloneFXDialog.scala
  // just for initialization:
  new JFXPanel()
  // Create a dialog stage and display it on JavaFX Application Thread
  Platform.runLater {
    val dialogStage = new Stage {
      outer => {
        title = message
        val password = new PasswordField
        password.setOnKeyPressed(
          new EventHandler[KeyEvent] {
            override def handle(t: KeyEvent) = if (t.getCode() == KeyCode.ENTER) {
              actr ! Password(password.text())
              outer.close()
            }
          }
        )
        password.setPrefWidth(300)
        scene = new Scene {
          root = password
        }
        password.requestFocus()
      }
    }
    dialogStage.setOnCloseRequest(
       new EventHandler[WindowEvent] {
         override def handle(event: WindowEvent) = {
           actr ! PasswordPromptClosed()
         }
       }
    )
    dialogStage.showAndWait()
  }
}