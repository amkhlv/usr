package controllers

/**
  * Created by andrei on 2/25/17.
  */

import java.io.{File, InputStream}
import java.time.{Instant, LocalDateTime, ZoneId}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import play.api.Configuration
import com.andreimikhailov.utils._
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.unixsocket.UnixSocketConnector
import play.api.libs.json.{JsValue, Json}
import scala.sys.process._

object EmacsClientLauncher {
  def launch(filepath:String) =
    Seq("emacsclient", filepath).run
}

class Handler(ical: ICal) extends AbstractHandler {
  var html = <h1>Something, innit</h1>

  override def handle(target: String,
                      req: Request,
                      httpReq: HttpServletRequest,
                      httpRes: HttpServletResponse) = {

    httpRes.setContentType("application/json")
    println("=== handling ===")
    val outWriter = httpRes.getWriter()
    val istrm: InputStream = req.getInputStream()
    val js : JsValue = Json.parse(
      scala.io.Source.fromInputStream(istrm).mkString
    )

    (js \ "isIntervalRequest").asOpt[Boolean] match {
      case Some(true) =>
        println("=== handling IntervalRequest ===")
        val startDateJs : JsValue = (js \ "dateFrom").get
        val startDate = ICal.jDate(
          (startDateJs \ "year").as[Int], (startDateJs \ "month").as[Int], (startDateJs \ "day").as[Int], 0, 0
        )
        println("=== start date : " + startDate.toString)
        val untilDateJs : JsValue = (js \ "dateUntil").get
        val untilDate = ICal.jDate(
          (untilDateJs \ "year").as[Int], (untilDateJs \ "month").as[Int], (untilDateJs \ "day").as[Int], 0, 0
        )
        val unfEvents = ical.eventsInRange(startDate, untilDate)
        httpRes.setStatus(HttpServletResponse.SC_OK)
        for (ev <- unfEvents) yield {
          val instant0 : Instant = ev.start.toInstant()
          val instant1 : Instant = ev.end.toInstant()
          val clndr0 = LocalDateTime.ofInstant(instant0, ZoneId.systemDefault())
          val clndr1 = LocalDateTime.ofInstant(instant1, ZoneId.systemDefault())
          val jsOut = Json.obj(
            "summary" -> ev.summary,
            "startDate" -> Json.obj(
              "year" -> clndr0.getYear(),
              "month" -> clndr0.getMonthValue(),
              "day"   -> clndr0.getDayOfMonth(),
              "hour" -> clndr0.getHour(),
              "minute" -> clndr0.getMinute(),
              "second" -> clndr0.getSecond(),
              "isDate" -> false
            ),
            "endDate" -> Json.obj(
              "year" -> clndr1.getYear(),
              "month" -> clndr1.getMonthValue(),
              "day"   -> clndr1.getDayOfMonth(),
              "hour" -> clndr1.getHour(),
              "minute" -> clndr1.getMinute(),
              "second" -> clndr1.getSecond(),
              "isDate" -> false
            ),
            "isRecurring" -> false,
            "isDerived" -> false
          )
          outWriter.write(Json.stringify(jsOut))
          outWriter.flush()
        }
      case _ => ()
    }
    (js \ "isEmacsClient").asOpt[Boolean] match {
      case Some(true) =>
        println("=== handling EmacsClient ===")
        httpRes.setStatus(HttpServletResponse.SC_OK)
        EmacsClientLauncher.launch((js \ "filename").as[String])
      case _ => ()
    }
    req.setHandled(true)
    outWriter.close()
  }
}


class MyJetty(config: Configuration) {

  val ical = ICal.iCalFromFile(new File(config.get[String]("application.ics")))
  val UDS = new File(config.get[String]("application.UDS"))

  def run() = {
    UDS.delete()
    val server = new Server()
    val connector : UnixSocketConnector = new UnixSocketConnector(server);
    connector.setAcceptQueueSize(128);
    connector.setUnixSocket(UDS.getAbsolutePath);
    server.addConnector(connector);
    server.setHandler(new Handler(ical))
    server.start
  }

}
