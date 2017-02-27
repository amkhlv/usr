package controllers

/**
  * Created by andrei on 2/26/17.
  */
import com.andreimikhailov.utils._

import scala.concurrent.{ExecutionContext, Future, blocking}
import scalafx.application.JFXApp.PrimaryStage

object Common {
  implicit val ec = ExecutionContext.global
  lazy val mainwin: PrimaryStage = {
    val mainJFX = new MainJFX("starting")
    Future{ blocking {mainJFX.main(Array())}}
    def getMainwin() : PrimaryStage = try {
      Thread.sleep(3000)
      mainJFX.bringDown()
      mainJFX.mainwin
    } catch {
      case e: Exception =>
        println(e.toString)
        println("--- waiting for mainwin to come online ---")
        getMainwin()
    }
    getMainwin()
  }

}
