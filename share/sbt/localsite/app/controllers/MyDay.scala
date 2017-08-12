package controllers

/**
  * Created by andrei on 12/24/16.
  */

import java.text.SimpleDateFormat
import java.util.{GregorianCalendar, Locale, Calendar => JCal}

import com.andreimikhailov.utils._
import com.andreimikhailov.utils.ICal._

object MyDay {
  def dayStart(jd: JDate): JCal = {
    val jcDayStart = new GregorianCalendar()
    jcDayStart.setTime(jd)
    jcDayStart.set(JCal.HOUR_OF_DAY, 0)
    jcDayStart.set(JCal.MINUTE, 0)
    jcDayStart.set(JCal.SECOND, 0)
    jcDayStart.set(JCal.MILLISECOND, 0)
    jcDayStart
  }
  def prevDayStart(jd: JDate): JCal = {
    val jcDayStart = new GregorianCalendar()
    jcDayStart.setTime(jd)
    jcDayStart.set(JCal.HOUR_OF_DAY, 0)
    jcDayStart.set(JCal.MINUTE, 0)
    jcDayStart.set(JCal.SECOND, 0)
    jcDayStart.set(JCal.MILLISECOND, 0)
    jcDayStart.add(JCal.DAY_OF_YEAR, -1)
    jcDayStart
  }
  def dayEnd(jd: JDate): JCal = {
    val jcDayEnd = new GregorianCalendar()
    jcDayEnd.setTime(jd)
    jcDayEnd.set(JCal.HOUR_OF_DAY, 0)
    jcDayEnd.set(JCal.MINUTE, 0)
    jcDayEnd.set(JCal.SECOND, 0)
    jcDayEnd.set(JCal.MILLISECOND, 0)
    jcDayEnd.add(JCal.DAY_OF_YEAR, 1)
    jcDayEnd
  }
}
class MyDay(jd: JDate, ues: List[UnfoldedEvent], highlightToday: Boolean = true) {

  private def jcNow = JCal.getInstance()
  private val jcGiven = new GregorianCalendar()
  jcGiven.setTime(jd)
  val yearInt: Int = jcGiven.get(JCal.YEAR)
  val monthInt: Int = jcGiven.get(JCal.MONTH)
  val month: String = jcGiven.getDisplayName(JCal.MONTH, JCal.LONG, Locale.getDefault())

  def isToday = (jcGiven.get(JCal.YEAR) == jcNow.get(JCal.YEAR)) &&
    (jcGiven.get(JCal.DAY_OF_YEAR) == jcNow.get(JCal.DAY_OF_YEAR))
  def events: List[UnfoldedEvent] = ues.filter(ue =>
    ue.start.before(MyDay.dayEnd(jd).getTime()) && ue.end.after(MyDay.dayStart(jd).getTime())
  )
  def style: String = if (highlightToday && isToday) "today" else "otherDay"
  val dayInt: Int = jcGiven.get(JCal.DAY_OF_MONTH)
  val dayTitle: String = jcGiven.get(JCal.DAY_OF_MONTH).toString()
  def timestr(ev: UnfoldedEvent): String =
    if (jcGiven.get(JCal.DAY_OF_YEAR) == MyDay.dayStart(jd).get(JCal.DAY_OF_YEAR)) {
      val jc = JCal.getInstance()
      jc.setTime(ev.start)
      val min = jc.get(JCal.MINUTE)
      jc.get(JCal.HOUR_OF_DAY) + ":" + (if (min.toInt > 9) min else "0"+min) + " "
    } else ""
  val summaries: List[String] = for (ev <- events) yield timestr(ev) + ev.summary
  val yyyymmdd: String = {
    val format = new SimpleDateFormat("yyyyMMdd")
    format.format(jcGiven.getTime)
  }
  val yyyy_mm_dd: String = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    format.format(jcGiven.getTime)
  }

}
