package controllers

import java.util
import java.util.{Calendar => JCal}
import javax.inject._

import akka.actor._
import akka.stream.Materializer
import com.andreimikhailov.utils._
import com.vladsch.flexmark.ast.Node
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ext.tables.TablesExtension
import play.Configuration
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import play.filters.csrf.{CSRFAddToken, CSRFCheck}
import collection.JavaConverters._
import scala.io.Source
import scala.sys.process.Process
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(val addToken: CSRFAddToken,
                               val checkToken: CSRFCheck,
                               val messagesApi: MessagesApi,
                               implicit val system: ActorSystem,
                               implicit val materializer: Materializer,
                               val config: Configuration
                              )
  extends Controller with I18nSupport {
  def getMarkdown: String = {
    val extensions = List(TablesExtension.create()).asJavaCollection
    val parser = Parser.builder().extensions(extensions).build();
    val mdown: String = Source.fromFile(config.getString("application.topmd")).getLines.mkString("\n")
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


  val broadcastActor = system.actorOf(Props(new SocketBroadcastActor(icalFileName)), "broadcastActor")
  val launchActor = system.actorOf(LaunchActor.props(broadcastActor), "launchActor")
  val watcher = new FileWatcher(
    icalDir,
    broadcastActor,
    {nm => (nm == icalFileName)},
    500
  )
  watcher.start
  def index = addToken {
    Action { implicit request =>
      Ok(views.html.index(getWeeks(1,2), sqliMap, getMarkdown))
    }
  }
  def calendar = addToken {
    Action { implicit  request =>
      Ok(views.html.calendar(getWeeks(1,12), sqliMap, getMarkdown))
    }
  }
  def socket = WebSocket.accept[JsValue, JsValue] { request =>
    ActorFlow.actorRef(out => MyWebSocketActor.props(out, broadcastActor))
  }
  def sqli(s:String) = checkToken {
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

}

