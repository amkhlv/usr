
package controllers

/**
  * Created by andrei on 12/24/16.
  */

import java.util.{Calendar => JCal}

import com.andreimikhailov.utils.ICal.JDate
import net.fortuna.ical4j.model.component._

class MyEvent(vev: VEvent) {
  
  val uid: String = vev.getUid.getValue
  val summary: String = vev.getSummary.getValue
  val start: JDate = vev.getStartDate.getDate
  val timezone: Option[String] = Option(vev.getStartDate.getTimeZone).map(_.getID)
  val end: JDate = vev.getEndDate.getDate
  val location: Option[String] = Option(vev.getLocation).map(_.getValue)
  val description: Option[String] = Option(vev.getDescription).map(_.getValue)

}

case class EventFormData(
                          uid: String,
                          summary: String,
                          start: String,
                          end: String,
                          location: String,
                          description: String
                        )

case class DeleteEventFormData (uid: String)