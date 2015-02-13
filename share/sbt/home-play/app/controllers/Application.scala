package controllers

import java.io.File

import org.markdown4j.Markdown4jProcessor
import play.api.{Configuration, Play}
import play.api.mvc.{Action, Controller}
import scala.sys.process._
import scala.language.postfixOps

object Application extends Controller {
  val ics: String = Play.current.configuration.getString("application.ics") match {
    case None => throw new Exception("conf parameter application.ics is missing from the file application.conf!")
    case Some(s) => s
  }
  val sqliConf: Configuration = Play.current.configuration.getConfig("application.sqlis") match {
    case None => throw new Exception("conf parameter application.sqlis is missing from the file application.conf!")
    case Some(c) => c
  }
  val sqliMap : Map[String,String] = (for ( k <- sqliConf.keys.toList ) yield (
    k -> (sqliConf.getString(k) match {
      case None => throw new Exception("error in sqli configuration")
      case Some(f) => f
    })     ) ) toMap
  def markdown: String = {
    val processor = new Markdown4jProcessor()
    processor.process(new File(
      Play.current.configuration.getString("application.topmd") match {
        case None => throw new Exception("conf parameter application.topmd is missing from the file application.conf!")
        case Some(s) => s
      }
    ))
  }
  def weeks(back: Int, forw: Int): List[List[MyDay]] = {
    val d = java.util.Calendar.getInstance()
    d.set(java.util.Calendar.SECOND, 0)
    d.set(java.util.Calendar.MINUTE, 0)
    d.set(java.util.Calendar.HOUR_OF_DAY, 0)
    d.set(java.util.Calendar.DAY_OF_WEEK, java.util.Calendar.MONDAY)
    val t : java.util.Date = d.getTime()
    t.setTime(t.getTime - back * 1000 * 3600 * 24 * 7 )
    for ( w <- List.range(0,back+forw) ) yield {
      for  ( dow <- List.range(0,7)) yield {
        new MyDay(new java.util.Date(t.getTime() + (dow + 7 * w) * 1000 * 3600 * 24))
      }
    }
  }
  def index = Action {
    Ok(views.html.index(weeks(1,2)))
  }

  def sqli(s:String) = Action {
    sqliMap.get(s) match {
      case None => Ok(views.html.index(weeks(1, 2)))
      case Some(x) => {
        Seq("/home/andrei/bin/amkhlv-x-launcher.sh", "/home/andrei/bin/linii2 -y " + x).run
        Ok(views.html.index(weeks(1, 2)))
      }
    }
  }

}

