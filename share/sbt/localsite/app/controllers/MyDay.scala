package controllers

/**
  * Created by andrei on 12/24/16.
  */

import java.util.{GregorianCalendar, Calendar => JCal, Locale}
import com.andreimikhailov.utils._
import com.andreimikhailov.utils.ICal._


class MyDay(jd: JDate, ues: List[UnfoldedEvent], highlightToday: Boolean = true) {

  private def jcNow = JCal.getInstance()
  private val jcGiven = new GregorianCalendar()
  jcGiven.setTime(jd)
  private val monthInt: Int = jcGiven.get(JCal.MONTH)
  val month: String = jcGiven.getDisplayName(JCal.MONTH, JCal.LONG, Locale.getDefault())
  private val jcDayStart = new GregorianCalendar()
  jcDayStart.setTime(jd)
  jcDayStart.set(JCal.HOUR_OF_DAY, 0)
  jcDayStart.set(JCal.MINUTE, 0)
  jcDayStart.set(JCal.SECOND, 0)
  jcDayStart.set(JCal.MILLISECOND, 0)
  private val jcDayEnd = new GregorianCalendar()
  jcDayEnd.setTime(jd)
  jcDayEnd.set(JCal.HOUR_OF_DAY, 0)
  jcDayEnd.set(JCal.MINUTE, 0)
  jcDayEnd.set(JCal.SECOND, 0)
  jcDayEnd.set(JCal.MILLISECOND, 0)
  jcDayEnd.add(JCal.DAY_OF_YEAR, 1)
  def isToday = (jcGiven.get(JCal.YEAR) == jcNow.get(JCal.YEAR)) &&
    (jcGiven.get(JCal.DAY_OF_YEAR) == jcNow.get(JCal.DAY_OF_YEAR))
  def events: List[UnfoldedEvent] = ues.filter(ue =>
    ue.start.before(jcDayEnd.getTime) && ue.end.after(jcDayStart.getTime)
  )
  def style: String = if (highlightToday && isToday) "today" else "otherDay"
  val dayTitle: String = jcGiven.get(JCal.DAY_OF_MONTH).toString()
  def timestr(ev: UnfoldedEvent): String = if (jcGiven.get(JCal.DAY_OF_YEAR) == jcDayStart.get(JCal.DAY_OF_YEAR)) {
    val jc = JCal.getInstance()
    jc.setTime(ev.start)
    val min = jc.get(JCal.MINUTE)
    return jc.get(JCal.HOUR_OF_DAY) + ":" + (if (min.toInt > 9) min else "0"+min) + " "
  } else return ""
  val summaries: List[String] = for (ev <- events) yield timestr(ev) + ev.summary


}
