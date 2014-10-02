package controllers

import java.text.DateFormat
import scala.language.postfixOps

/**
 * Created by andrei on 28/09/14.
 */
class MyDay(day: java.util.Date) {
  val events = ICalendar.getEventsForDay(day)
  private val clndr: java.util.Calendar = java.util.Calendar.getInstance();
  private val today: String = clndr.get(java.util.Calendar.DAY_OF_MONTH).toString
  clndr.setTime(day)
  val dayTitle: String = clndr.get(java.util.Calendar.DAY_OF_MONTH).toString
  val summaries: List[String] = (
    for ( ev <- events ) yield ev.getProperty(net.fortuna.ical4j.model.Property.SUMMARY).getValue
    ) toList
  val style = if (dayTitle == today) "today" else "otherDay"
}

class MyEvent()
