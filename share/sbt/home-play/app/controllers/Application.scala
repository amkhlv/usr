package controllers

import java.text.SimpleDateFormat
import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.{Configuration, Play}
import org.markdown4j.Markdown4jProcessor
import java.io.File
import scala.sys.process.Process

object Application {
  val ics: String = current.configuration.getString("application.ics") match {
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
  val xauthority: String = Play.current.configuration.getString("application.xauthority") match {
    case None => "/home/andrei/.Xauthority"
    case Some(s) => s
  }

  val display: String = Play.current.configuration.getString("application.display") match {
    case None => ":0.0"
    case Some(s) => s
  }

  val sqliConf: Configuration = Play.current.configuration.getConfig("application.sqlis") match {
    case None => throw new Exception("conf parameter application.sqlis is missing from the file application.conf!")
    case Some(c) => c
  }
  def sqliMap : Map[String,String] = (for ( k <- sqliConf.keys.toList ) yield
    k -> (sqliConf.getString(k) match {
      case None => throw new Exception("error in sqli configuration")
      case Some(f) => f
    })      ) toMap
}

class Application extends Controller {
  private def timeMondayStarts(relNow: Long): Long = {
    val d = java.util.Calendar.getInstance()
    d.set(java.util.Calendar.SECOND, 0)
    d.set(java.util.Calendar.MINUTE, 0)
    d.set(java.util.Calendar.HOUR_OF_DAY, 0)
    d.set(java.util.Calendar.DAY_OF_WEEK, java.util.Calendar.MONDAY)
    val t : Long = d.getTimeInMillis
    val result : Long = t + relNow * 1000 * 3600 * 24 * 7
    result
  }
  private def weeksRemainingInMonth(t: Long): Long = {
    val cal = java.util.Calendar.getInstance()
    cal.setTimeInMillis(t)
    cal.set(java.util.Calendar.DAY_OF_WEEK, java.util.Calendar.MONDAY)
    val monthNow = cal.get(java.util.Calendar.MONTH)
    def remaining(x: Long): Long = {
      cal.add(java.util.Calendar.WEEK_OF_YEAR, 1)
      if (cal.get(java.util.Calendar.MONTH) == monthNow) {
        remaining(x + 1)
      } else x + 1
    }
    remaining(0)
  }
  private def monthName(t: Long): String = {
    val cal = java.util.Calendar.getInstance()
    cal.setTimeInMillis(t)
    new SimpleDateFormat("MMMM").format(cal.getTime())
  }
  def weeks(back: Long, forw: Long, highlightToday: Boolean = true): List[List[MyDay]] =
    for ( w: Long <- List.range(0,back+forw) ) yield {
      for  ( dow <- List.range(0.toLong,7.toLong)) yield {
        new MyDay(
          new java.util.Date(timeMondayStarts( - back) + (dow + (7.toLong) * w) * 1000 * 3600 * 24),
          highlightToday
        )
      }
    }


  def months(n: Int) : List[MyMonth] = months(n, -1)


  private def months(n: Int, mondayRelNow: Long): List[MyMonth] = {
    val mn: Long =  timeMondayStarts(mondayRelNow)
    val nwks: Long = weeksRemainingInMonth(mn)
    if (n>0) {
      MyMonth(
        monthName(mn),
        weeks(- mondayRelNow, mondayRelNow + nwks, false)) :: months(n - 1, mondayRelNow + nwks)
    } else {
      List()
    }
  }

  def index = Action {
    Ok(views.html.index(weeks(1,2)))
  }

  def calendar = Action {
    Ok(views.html.calendar(months(5)))
  }

  def sqli(s:String) = Action {
    Application.sqliMap.get(s) match {
      case None => Ok(views.html.index(weeks(1, 2)))
      case Some(x) => {
        Process(Seq(
          "/home/andrei/bin/linii2",
          "-y",
          x
        ),
          None,
          ("DISPLAY",
          Application.display),
          ("XAUTHORITY",
          Application.xauthority)).run
        Ok(views.html.index(weeks(1, 2)))
      }
    }
  }
}

case class MyMonth(title: String, days: List[List[MyDay]])
