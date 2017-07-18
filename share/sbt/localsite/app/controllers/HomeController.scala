package controllers

import java.util.{Calendar => JCal}
import javax.inject._

import akka.actor._
import akka.stream.Materializer
import com.andreimikhailov.utils._
import com.vladsch.flexmark.ast.Node
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import play.Configuration
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow
import play.api.mvc._

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
class HomeController @Inject()(val messagesApi: MessagesApi,
                               implicit val system: ActorSystem,
                               implicit val materializer: Materializer,
                               val config: Configuration,
                               val webJarAssets: WebJarAssets
                              )
  extends Controller with I18nSupport {
  implicit val ec = ExecutionContext.global
  Future { blocking { (new MyJetty(config)).run() }}
  


  def getMarkdown: String = {
    val extensions = List(TablesExtension.create()).asJavaCollection
    val parser = Parser.builder().extensions(extensions).build();
    val mdown: String = Source.fromFile(config.getString("application.topmd")).getLines.mkString("\n")
    val document: Node = parser.parse(mdown)
    val renderer = HtmlRenderer.builder().extensions(extensions).build();
    return renderer.render(document);
  }
  def getMarkdownForCalendar: String  = {
    val extensions = List(TablesExtension.create()).asJavaCollection
    val parser = Parser.builder().extensions(extensions).build();
    val mdown: String = Source.fromFile(config.getString("application.calmd")).getLines.mkString("\n")
    val document: Node = parser.parse(mdown)
    val renderer = HtmlRenderer.builder().extensions(extensions).build();
    return renderer.render(document);
  }
  val icalFile  = new java.io.File(config.getString("application.ics"))
  val icalDir = icalFile.getParentFile.toPath
  val icalFileName : String = icalFile.toPath.getFileName.toString
  def getWeeks(back: Int, forw: Int): List[List[MyDay]] = {
    val ical = ICal.iCalFromFile(icalFile)
    val now = JCal.getInstance().getTime
    val evs = ical.eventsInRange(
      ICal.weekShiftBy(-(back + 1), ICal.getThisDayOfWeekFor(now, 1)),
      ICal.weekShiftBy(forw + 1, ICal.getThisDayOfWeekFor(now, 1))
    )
    return for (j <- ((- back) to forw).toList) yield {
      for (i <- List(1, 2, 3, 4, 5, 6, 7)) yield new MyDay(
        ICal.weekShiftBy(j, ICal.getThisDayOfWeekFor(now, i)),
        evs
      )
    }
  }
  val sqliMap : java.util.Map[String,AnyRef] = config.getConfig("application.sqlis").asMap()
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
  val watcher = new FileWatcher(
    icalDir,
    broadcastActor,
    {nm => (nm == icalFileName)},
    500
  )
  watcher.start
  def index =
    Action { implicit request =>
      Ok(views.html.index(getWeeks(1,2), sqliMap, getMarkdown, isSecure, webJarAssets))
    }
  def calendar =
    Action { implicit  request =>
      Ok(views.html.calendar(getWeeks(1,12), sqliMap, getMarkdownForCalendar))
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
          config.getConfig("application.sqlis").getString(s)
        ),
        None,
        ("DISPLAY", config.getString("application.display")),
        ("XAUTHORITY", config.getString("application.xauthority"))
      )
      Redirect("/")
    }
}

