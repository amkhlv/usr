package com.andreimikhailov.utils

/**
  * Created by andrei on 12/20/16.
  */

import java.io.FileInputStream
import java.util.{Collection, GregorianCalendar, Calendar => JCal}

import com.andreimikhailov.utils.ICal.JDate
import net.fortuna.ical4j.data._
import net.fortuna.ical4j.filter._
import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component.{CalendarComponent, VEvent}
import net.fortuna.ical4j.model.property.{CalScale, ProdId, Uid, Version}

import scala.collection.JavaConversions._
import scala.collection.mutable;

/**
  * @constructor
  * @param start
  * @param end
  * @param vev
  * @param isRecurrent
  */
class UnfoldedEvent(val start:JDate, val end:JDate, val vev:VEvent, val isRecurrent: Boolean) {
  val summary: String = vev.getSummary().getValue
}
object ICal {
  type JDate = java.util.Date
  val gJC: JCal = new GregorianCalendar()
  def iCalFromFile(f: java.io.File) : ICal = {
    val builder = new CalendarBuilder()
    val calendar: Calendar = builder.build(new FileInputStream((f)))
    return  new ICal(calendar)
  }
  def jDate(year:Int, month:Int, day:Int, hour:Int, minute:Int): JDate = {
    val jc = new java.util.GregorianCalendar()
    jc.set(JCal.YEAR, year)
    jc.set(JCal.MONTH, month - 1)
    jc.set(JCal.DAY_OF_MONTH, day)
    jc.set(JCal.HOUR_OF_DAY, hour)
    jc.set(JCal.MINUTE, minute)
    jc.set(JCal.SECOND, 0)
    jc.set(JCal.MILLISECOND, 0)
    return jc.getTime
  }
  def getThisDayOfWeekFor(jd: JDate, i:Int): JDate = {
    if ((i<1) || (i>7)) throw new Exception("The numeration of days goes from 1 to 7")
    var jc: JCal = new GregorianCalendar() // this is datetime right now
    jc.setTime(jd)
    while (jc.get(JCal.DAY_OF_WEEK) != jc.getFirstDayOfWeek) jc.add(JCal.DAY_OF_YEAR, -1)
    jc.add(JCal.DAY_OF_YEAR, i - 1)
    jc.set(JCal.HOUR_OF_DAY,0)
    jc.set(JCal.MINUTE,0)
    jc.set(JCal.SECOND,0)
    jc.set(JCal.MILLISECOND, 0)
    return jc.getTime
  }
  def getStartOfNextDay(jd: JDate) : JDate = {
    var jc: JCal = new GregorianCalendar()
    jc.setTime(jd)
    jc.add(JCal.DAY_OF_YEAR, 1)
    jc.set(JCal.HOUR_OF_DAY,0)
    jc.set(JCal.MINUTE,0)
    jc.set(JCal.SECOND,0)
    jc.set(JCal.MILLISECOND, 0)
    return jc.getTime
  }
  def weekShiftBy(i:Int, jd: JDate) : JDate = {
    var jc: JCal = new GregorianCalendar()
    jc.setTime(jd)
    jc.add(JCal.DAY_OF_YEAR, 7*i)
    return jc.getTime
  }
  private def getNamesOfDaysOfWeek(style: Int): List[String] = {
    val jcwk = new java.util.GregorianCalendar()
    jcwk.set(JCal.DAY_OF_WEEK, jcwk.getFirstDayOfWeek)
    jcwk.add(JCal.DAY_OF_YEAR, -1)
    for (i <- (0 to 6).toList) yield {
      jcwk.add(JCal.DAY_OF_YEAR, 1)
      jcwk.getDisplayName(JCal.DAY_OF_WEEK, JCal.LONG, java.util.Locale.getDefault())
    }
  }
  val namesOfDaysOfWeek: List[String] =  getNamesOfDaysOfWeek(JCal.LONG)
  val namesShortOfDaysOfWeek: List[String] = getNamesOfDaysOfWeek(JCal.SHORT)
  def getUID(vev: VEvent): String = vev.getProperties().getProperty(Property.UID).getValue()
}

/**
  * a helper to process ical files
  * @constructor
  * @param calendar  ICalendar to process
  */
class ICal(val calendar: Calendar) {
  def getMatches(clcs: Collection[Any], rls: Array[Rule[Any]]): Collection[Any] = {
      val filter = new Filter(rls, Filter.MATCH_ALL)
      return filter.filter(clcs)
    }
  def eventsInRange(start: JDate, end: JDate): List[UnfoldedEvent] = {
    val period: Period = new Period(new DateTime(start), new DateTime(end))
    val ms = getMatches(
      calendar.getComponents.asInstanceOf[Collection[Any]],
      Array(new PeriodRule(period)).asInstanceOf[Array[Rule[Any]]]
    )
    var mm: mutable.MutableList[UnfoldedEvent] = mutable.MutableList()
    for (c <- ms.iterator()) c match {
      case vev: VEvent =>
        val fallingIn: PeriodList = vev.calculateRecurrenceSet(period)
        if (fallingIn.isEmpty()) {
          mm.+=:(new UnfoldedEvent(vev.getStartDate.getDate, vev.getEndDate.getDate, vev, false))
        } else {
          for (p <- fallingIn.iterator()) p match {
            case prd: Period =>
              mm.+=:(new UnfoldedEvent(prd.getStart, prd.getEnd, vev, true))
          }
        }
      case _ => ()
    }
    return mm.toList.sortWith((x,y) => x.start.before(y.start))
  }
  def eventWithUID(uid: String): Option[VEvent] = {
    val myUIDRule = new HasPropertyRule(new Uid(uid))
    val ms = getMatches(
      calendar.getComponents.asInstanceOf[Collection[Any]],
      Array(myUIDRule).asInstanceOf[Array[Rule[Any]]]
    )
    ms.headOption.flatMap(_ match { case v: VEvent => Some(v) case _ => None })
  }
  def updateEvent(uid: String, newvev: VEvent): Calendar = {
    val cs = calendar.getComponents.asInstanceOf[Collection[Any]]
    val newcs = new ComponentList[CalendarComponent]().asInstanceOf[Collection[Any]]
    for (c <- cs) {
       if (c.isInstanceOf[VEvent]) {
         val v = c.asInstanceOf[VEvent]
         if (v.getUid.getValue == uid) {
           newcs.add(newvev)
         } else { newcs.add(v) }
       }
    }
    val a = new Calendar(newcs.asInstanceOf[ComponentList[CalendarComponent]])
    a.getProperties().add(new ProdId("-//Ben Fortuna//iCal4j 1.0//EN"))
    a.getProperties().add(Version.VERSION_2_0)
    a.getProperties().add(CalScale.GREGORIAN)
    a
  }
  def deleteEvent(uid: String): Calendar = {
    val cs = calendar.getComponents.asInstanceOf[Collection[Any]]
    val newcs = new ComponentList[CalendarComponent]().asInstanceOf[Collection[Any]]
    for (c <- cs) {
       if (c.isInstanceOf[VEvent]) {
         val v = c.asInstanceOf[VEvent]
         if (v.getUid.getValue != uid) newcs.add(v) 
       }
    }
    val a = new Calendar(newcs.asInstanceOf[ComponentList[CalendarComponent]])
    a.getProperties().add(new ProdId("-//Ben Fortuna//iCal4j 1.0//EN"))
    a.getProperties().add(Version.VERSION_2_0)
    a.getProperties().add(CalScale.GREGORIAN)
    a
  }
  def insertNewEventInPlace(newvev: VEvent) = {
    calendar.getComponents.add(newvev)
  }

}


