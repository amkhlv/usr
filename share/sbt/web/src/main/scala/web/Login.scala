package web

import web.utils.{Decryptor, LoginRobot}

import scala.App

object Login extends App {
  println(s"-- Preparing to login to ${args.toList(0)}")
  val x = xml.XML.loadString(Decryptor.decrypt)
  val sites = (x \\ "site").map {s => new MyPassItem(s)}.toList.filter(
    site =>  site.nick == args.toList(0)
  )
  for (site <- sites) yield {
    site.selenium match {
      case Some(sel) => for ( acc <- site.accounts ) yield {
        println(s"-- Logging in to ${site.nick} as ${acc.attribute("login")}")
        new LoginRobot(acc, sel) go
      }
      case None => println("No Selenium for the site")
    }
  }

}
