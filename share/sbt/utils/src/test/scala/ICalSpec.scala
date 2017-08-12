/**
  * Created by andrei on 12/22/16.
  */
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import net.fortuna.ical4j.data.CalendarBuilder
import org.scalatest._
import com.andreimikhailov.utils._
object ICalSpec {
  val example =
    """
      |BEGIN:VCALENDAR
      |BEGIN:VEVENT
      |CREATED:20140627T193030Z
      |LAST-MODIFIED:20151231T133357Z
      |DTSTAMP:20151231T133357Z
      |UID:20111113T210730Z-25643-1000-1-5@limao
      |SUMMARY:colloquium
      |STATUS:CONFIRMED
      |RRULE:FREQ=DAILY;INTERVAL=7
      |DTSTART;TZID=America/Sao_Paulo:20140709T140000
      |DTEND;TZID=America/Sao_Paulo:20140709T153000
      |END:VEVENT
      |BEGIN:VEVENT
      |UID:20161220T170016Z-913-1000-682-0@arado
      |DTSTAMP:20161220T165622Z
      |DTSTART;TZID=/freeassociation.sourceforge.net/Tzfile/America/Sao_Paulo:20161129T103000
      |DTEND;TZID=/freeassociation.sourceforge.net/Tzfile/America/Sao_Paulo:20161129T110000
      |TRANSP:OPAQUE
      |SEQUENCE:2
      |SUMMARY:single example
      |CLASS:PUBLIC
      |CREATED:20161220T170042Z
      |LAST-MODIFIED:20161220T170042Z
      |END:VEVENT
      |END:VCALENDAR

    """.stripMargin
  val builder = new CalendarBuilder()
  val calendar = builder.build(new ByteArrayInputStream(example.getBytes(StandardCharsets.UTF_8)))
  val myICal = new ICal(calendar)
}
class ICalSpec extends FlatSpec with Matchers {
  import ICalSpec._
  "ICal" should "correctly find events in a given range" in {
    myICal.eventsInRange(ICal.jDate(2016,11,1,0,0),ICal.jDate(2016,12,1,0,0)) should have length 6
  }
  it should "have right number of components" in {
    calendar.getComponents() should have length 2
  }
  it should "correctly find the first day of week for a given date" in {
    ICal.getThisDayOfWeekFor(ICal.jDate(2016,12,24,0,0), 1) should (
      equal (ICal.jDate(2016,12,18,0,0)) or equal(ICal.jDate(2016,12,19,0,0))
      )
  }

}
