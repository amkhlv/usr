package com.andreimikhailov.utils

import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.text.Text
/**
  * Created by andrei on 2/26/17.
  */
class MainJFX(val message: String) extends JFXApp  {
  Platform.implicitExit = false
  val mainwin: PrimaryStage = new PrimaryStage {
    title.value = message
    scene = new Scene {
      content = new VBox {
        children = new Text {
            text = "starting"
            style = "-fx-font-size: 48pt"
            fill = new LinearGradient(
              endX = 0,
              stops = Stops(Cyan, DodgerBlue)
            )
            effect = new DropShadow {
              color = DodgerBlue
              radius = 25
              spread = 0.25
            }
          }
      }
    }
  }
  def bringUp() = Platform.runLater { mainwin.show()}
  def bringDown() = Platform.runLater { mainwin.hide() }
}
