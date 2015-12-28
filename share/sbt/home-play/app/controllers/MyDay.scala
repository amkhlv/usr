package controllers

/**
  * Created by andrei on 26/12/15.
  */

import scala.language.postfixOps

class MyDay(day: java.util.Date, highlightToday: Boolean = true) {
  val events = ICalendar.getEventsForDay(day)
  private val clndr: java.util.Calendar = java.util.Calendar.getInstance();
  private val today: String = clndr.get(java.util.Calendar.DAY_OF_MONTH).toString
  clndr.setTime(day)
  val dayTitle: String = clndr.get(java.util.Calendar.DAY_OF_MONTH).toString
  def dateToCalendar(d: java.util.Date): java.util.Calendar = {
    val cal = java.util.Calendar.getInstance()
    cal.setTime(d)
    cal
  }
  def show(ev: net.fortuna.ical4j.model.Component): String = {
    val s = ev.getProperty(net.fortuna.ical4j.model.Property.SUMMARY).getValue
    val dtstart: net.fortuna.ical4j.model.property.DtStart =
      ev.getProperty(net.fortuna.ical4j.model.Property.DTSTART) match {
        case d: net.fortuna.ical4j.model.property.DtStart => d
        case _ => throw new ClassCastException
      }
    val d: net.fortuna.ical4j.model.Date = dtstart.getDate
    val hh: Int = dateToCalendar(d).get(java.util.Calendar.HOUR_OF_DAY)
    val mm: Int = dateToCalendar(d).get(java.util.Calendar.MINUTE)
    hh.toString + ":" + (if (mm>9) mm.toString else "0" + mm.toString) + " " + s
  }
  val summaries: List[String] = (
    for ( ev <- events ) yield show(ev)
    ) toList
  val style = if (highlightToday && (dayTitle == today)) "today" else "otherDay"

}
