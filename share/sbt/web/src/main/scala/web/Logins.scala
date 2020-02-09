package web

import javafx.application.{Application, Platform}
import javafx.event.ActionEvent
import javafx.scene.Scene
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{HBox, VBox}
import javafx.stage.Stage
import web.utils.Decryptor

import scala.xml.{Node, NodeSeq}


class Logins extends Application {
  val x = xml.XML.loadString(Decryptor.decrypt)
  val sites = (x \\ "site").map {s => new MyPassItem(s)}.toList
  override def start(primaryStage: Stage) {
    primaryStage.setTitle("web logins")
    val root = new VBox
    primaryStage.setScene(new Scene(root, 700, 900))
    for (site <- sites) {
      if (!site.selenium.isEmpty) {
          val row = new HBox
          val siteLabel = new Label(site.nick)
          row.getChildren.add(siteLabel)
          val acctCol = new VBox
          for (login <- site.logins) {
            val btnAcct = new Button(login)
            btnAcct.setOnAction(
              (e: ActionEvent) => {
                new LoginRobot(site.account(login), site.selenium).go
              }
            )
            row.getChildren.add(btnAcct)
          }
          root.getChildren.add(row)
      }
    }
    val btnExit = new Button
    btnExit.setText("EXIT")
    btnExit.setOnAction((e: ActionEvent) => {
      Platform.exit()
    })
    root.getChildren.add(btnExit)
    primaryStage.show()
  }
}

object Logins {
    def main(args: Array[String])
    {
        Application.launch(classOf[Logins], args: _*)
        System.exit(0)
    }
}

