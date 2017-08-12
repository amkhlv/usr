package controllers

import java.text.SimpleDateFormat
import java.util.{Calendar => JCal}
import javax.inject._

import akka.actor._
import akka.stream.Materializer
import com.andreimikhailov.utils._
import com.vladsch.flexmark.ast.Node
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import net.fortuna.ical4j.model.component.VEvent
import net.fortuna.ical4j.model.property.DtStart
import play.api.Configuration
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.I18nSupport
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow
import play.api.mvc._

import scala.util.Random
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.io.Source
import scala.sys.SystemProperties
import scala.sys.process.Process
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(
                               cc: ControllerComponents,
                               implicit val system: ActorSystem,
                               implicit val materializer: Materializer,
                               val config: Configuration,
                               val webJarsUtil: org.webjars.play.WebJarsUtil
                              )
  extends AbstractController(cc) with I18nSupport {
  implicit val ec = ExecutionContext.global
  Future { blocking { (new MyJetty(config)).run() }}
  
  def getMarkdown: String = {
    val extensions = List(TablesExtension.create()).asJavaCollection
    val parser = Parser.builder().extensions(extensions).build();
    val mdown: String = Source.fromFile(config.get[String]("application.topmd")).getLines.mkString("\n")
    val document: Node = parser.parse(mdown)
    val renderer = HtmlRenderer.builder().extensions(extensions).build();
    return renderer.render(document);
  }
  def getMarkdownForCalendar: String  = {
    val extensions = List(TablesExtension.create()).asJavaCollection
    val parser = Parser.builder().extensions(extensions).build();
    val mdown: String = Source.fromFile(config.get[String]("application.calmd")).getLines.mkString("\n")
    val document: Node = parser.parse(mdown)
    val renderer = HtmlRenderer.builder().extensions(extensions).build();
    return renderer.render(document);
  }
  val icalFile  = new java.io.File(config.get[String]("application.ics"))
  val icalDir = icalFile.getParentFile.toPath
  val icalFileName : String = icalFile.toPath.getFileName.toString
  def getWeeksForDate(back: Int, forw: Int, dt: java.util.Date): List[List[MyDay]] = {
    val ical = ICal.iCalFromFile(icalFile)
    val evs = ical.eventsInRange(
      ICal.weekShiftBy(-(back + 1), ICal.getThisDayOfWeekFor(dt, 1)),
      ICal.weekShiftBy(forw + 1, ICal.getThisDayOfWeekFor(dt, 1))
    )
    return for (j <- ((- back) to forw).toList) yield {
      for (i <- List(1, 2, 3, 4, 5, 6, 7)) yield new MyDay(
        ICal.weekShiftBy(j, ICal.getThisDayOfWeekFor(dt, i)),
        evs
      )
    }
  }
  def getWeeks(back: Int, forw: Int): List[List[MyDay]] = getWeeksForDate(back, forw, JCal.getInstance().getTime)
  def getDay(y:Int, m:Int, d:Int) = {
    val ical = ICal.iCalFromFile(icalFile)
    val dt = ICal.jDate(y,m,d,0,0)
    val evs = ical.eventsInRange(MyDay.dayStart(dt).getTime(), MyDay.dayEnd(dt).getTime())
    val day = new MyDay(dt, evs)
    day
  }
  def getEventByUID(uid: String): Option[VEvent] = {
    val ical = ICal.iCalFromFile(icalFile)
    ical.eventWithUID(uid)
  }
  val sqliMap : Map[String,String] = config.get[Map[String,String]]("application.sqlis")
  val maybePort = {
    val sprops = new SystemProperties()
    sprops.get("http.port")
  }
  val maybeSPort = {
    val sprops = new SystemProperties()
    sprops.get("https.port")
  }
  val isSecure = maybeSPort match {
    case Some(x) => true
    case None => false
  }
  val broadcastActor = system.actorOf(Props(new SocketBroadcastActor(icalFileName)), "broadcastActor")
  val launchActor = system.actorOf(LaunchActor.props(broadcastActor), "launchActor")
  val calendarUpdateActor = system.actorOf(Props(new CalendarUpdateActor(config)), "calendarUpdateActor")
  val watcher = new FileWatcher(
    icalDir,
    broadcastActor,
    {nm => (nm == icalFileName)},
    500
  )
  watcher.start
  def index =
    Action { implicit request =>
      Ok(views.html.index(getWeeks(1,2), sqliMap, getMarkdown, isSecure, webJarsUtil))
    }
  def calendar =
    Action { implicit  request =>
      Ok(views.html.calendar(getWeeks(1,12), sqliMap, getMarkdownForCalendar))
    }
  def day(y:Int, m:Int, d:Int) = Action {
    implicit request => {
      Ok(views.html.day(getDay(y,m,d), isSecure, webJarsUtil))
    }
  }
  def monthGET(y:Int, m:Int) = Action {
    implicit request => {
      val c = new java.util.GregorianCalendar()
      c.set(y,m - 1,1)
      Ok(views.html.month(
        (new SimpleDateFormat("MMMM").format(c.getTime())) + f" $y%d",
        getWeeksForDate(0, 5, c.getTime),
        isSecure,
        webJarsUtil
      ))
    }
  }
  val editEventForm = Form(
    mapping(
      "uid"     -> text,
      "summary" -> text,
      "start"   -> text,
      "end"     -> text,
      "location" -> text,
      "description" -> text
    )(EventFormData.apply)(EventFormData.unapply)
  )
  def editEventGET(uid: String) = Action {
    implicit request => {
      getEventByUID(uid) match {
        case Some(u) => {
          val myevent = new MyEvent(u)
          val evForm = editEventForm.fill(EventFormData(
            uid,
            myevent.summary,
            myevent.start.toString,
            myevent.end.toString,
            myevent.location.getOrElse[String](""),
            myevent.description.getOrElse[String]("")
          ))
          Ok(views.html.editEvent(uid, myevent, evForm))
        }
        case None => Ok(views.html.error("UID not found"))
      }
    }
  }
  val editEventPOST = Action {
    implicit request => editEventForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.error("bad form data"))
      },
      validData => {
        val date8601 = """(\d\d\d\d)(\d\d)(\d\d)T(\d\d)(\d\d)(\d\d)""".r
        validData.start match {
          case date8601(yyyy,mm,dd,hour,min,sec) => {
            calendarUpdateActor ! UpdateEvent(icalFile, validData.uid, validData)
            Redirect("/" + yyyy + "-" + mm + "-" + dd)
          }
        }
      }
    )
  }
  val delEventForm = Form(mapping("uid" -> text)(DeleteEventFormData.apply)(DeleteEventFormData.unapply))
  def delEventGET(uid: String) = Action {
    implicit request => {
      getEventByUID(uid) match {
        case Some(u) => {
          val myevent = new MyEvent(u)
          Ok(views.html.delEvent(myevent))
        }
        case None => Ok(views.html.error("UID not found"))
      }
    }
  }
  val delEventPOST = Action {
    implicit request => delEventForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.error("bad form data"))
      },
      validData => {
        calendarUpdateActor ! DelEvent(icalFile, validData.uid)
        Redirect("/")
      }
    )
  }
  def newEventGET(t: String) = Action {
    implicit request => {
      val x = new DtStart(t)
      val cal = new java.util.GregorianCalendar()
      cal.setTime(x.getDate())
      cal.add(java.util.Calendar.HOUR,1)
      cal.add(java.util.Calendar.MINUTE,30)
      val dtEnd = new DtStart(new net.fortuna.ical4j.model.DateTime(cal.getTime()))
      val evForm = editEventForm.fill(EventFormData(
        s"amkhlv-${(new Random).alphanumeric take 16 mkString("")}", "", t, dtEnd.getValue, "", ""
      ))
      Ok(views.html.newEvent(evForm))
    }
  }
  def newEventPOST = Action {
    implicit request => editEventForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.error("bad form data"))
      },
      validData => {
        val date8601 = """(\d\d\d\d)(\d\d)(\d\d)T(\d\d)(\d\d)(\d\d)""".r
        validData.start match {
          case date8601(yyyy,mm,dd,hour,min,sec) => {
            calendarUpdateActor ! NewEvent(icalFile, validData)
            Redirect("/" + yyyy + "-" + mm + "-" + dd)
          }
        }
      }
    )
  }
  def socket = WebSocket.acceptOrResult[JsValue, JsValue] { request =>
    val trustedOrigin = maybeSPort match {
      case Some(i) => "https://localhost:" + i
      case None => maybePort match {
        case Some(i) => "http://localhost:" + i
        case None => "http://localhost"
      }
    }
    Future.successful(request.headers.get("Origin") match {
      case None => {
        println("=== ORIGIN HEADER ABSENT? ===")
        Left(Forbidden)
      }
      case Some(x) => if (x == trustedOrigin)  {
        println("=== GAVE SOCKET to --->" + x + "<--- ===")
        Right(ActorFlow.actorRef(out => MyWebSocketActor.props(out, broadcastActor)))
      } else {
        println("=== WRONG ORIGIN: --->" + x + "<---")
        Left(Forbidden)
      }
    })
  }
  def sqli(s:String) =
    Action { implicit request =>
      launchActor ! Process(
        Seq(
          "/home/andrei/bin/linii2",
          "-y",
          sqliMap(s)
        ),
        None,
        ("DISPLAY", config.get[String]("application.display")),
        ("XAUTHORITY", config.get[String]("application.xauthority"))
      )
      Redirect("/")
    }

}

