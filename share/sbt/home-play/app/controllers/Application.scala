package controllers

import java.io.{FileInputStream, File}
import java.util
import play.api.Play
import play.api.mvc.{Action, Controller}
import org.markdown4j.Markdown4jProcessor
import net.fortuna.ical4j.data.CalendarBuilder
import net.fortuna.ical4j.model.{Calendar, Period, DateTime, Dur, Component, ComponentList}
import net.fortuna.ical4j.filter.{PeriodRule, Filter}
import scala.language.postfixOps

object Application extends Controller {
  val ics: String = Play.current.configuration.getString("application.ics") match {
    case None => throw new Exception("conf parameter application.ics is missing from the file application.conf!")
    case Some(s) => s
  }
  val markdown: String = {
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
}

