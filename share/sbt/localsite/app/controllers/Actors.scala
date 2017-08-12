package controllers

/**
  * Created by andrei on 12/13/16.
  */

import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar

import akka.actor._
import com.andreimikhailov.utils.{FileEvent, ICal}
import controllers.MyWebSocketActor.{RegisterMe, UnRegisterMe}
import net.fortuna.ical4j.data.CalendarOutputter
import net.fortuna.ical4j.model.component.VEvent
import net.fortuna.ical4j.model.property._
import net.fortuna.ical4j.model.{DateTime, PropertyList}
import play.api.Configuration
import play.api.libs.json.{JsObject, JsString, JsValue}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.sys.process.Process

case class InternalMessage(j: JsValue)

object LaunchActor {
  def props(broadcast: ActorRef) = Props(new LaunchActor(broadcast))
}
class LaunchActor(broadcast: ActorRef) extends Actor {
  import sys.process._
  def receive = {
    case p: ProcessBuilder => p.run
  }
}

class CalendarUpdateActor(config: Configuration) extends Actor {
  def zenity(greeting: String) = Process(
        Seq(
          "/usr/bin/zenity",
          "--list",
          "--title", "Asking Permission",
          "--text",  greeting,
          "--radiolist",
          "--column", "", "--column", "",
          "",              "yes",
          "",              "no"
        ),
        None,
        ("DISPLAY", config.get[String]("application.display")),
        ("XAUTHORITY", config.get[String]("application.xauthority"))
      )
  def zenwarn(answer: String) = Process(
          Seq(
            "/usr/bin/zenity",
            "--warning",
            "--text", " calendar modification not allowed because answer was: «" + answer + "»"
          ),
          None,
          ("DISPLAY", config.get[String]("application.display")),
          ("XAUTHORITY", config.get[String]("application.xauthority"))
        )

    
  
  def receive = {
    case UpdateEvent(icalFile: java.io.File, uid: String, efd: EventFormData) => {
      val answer: String = zenity("allow ical update?").!!.stripLineEnd
      if (answer == "yes") {
        val ical = ICal.iCalFromFile(icalFile)
        val newprops = new PropertyList()
        newprops.add(new Uid(uid))
        newprops.add(new DtStamp(new DateTime()))
        newprops.add(new Summary(efd.summary))
        newprops.add(new DtStart(efd.start))
        newprops.add(new DtEnd(efd.end))
        if (efd.description != "") newprops.add(new Description(efd.description))
        if (efd.location != "") newprops.add(new Location(efd.location))
        val newvev = new VEvent(newprops)
        val newcal = ical.updateEvent(uid, newvev)
        val outputter = new CalendarOutputter(true)
        outputter.output(newcal, new FileWriter(icalFile))
      } else zenwarn(answer).run()
    }
    case NewEvent(icalFile: java.io.File, efd: EventFormData) => {
      val answer: String = zenity("allow new event?").!!.stripLineEnd
      if (answer == "yes") {
        val ical = ICal.iCalFromFile(icalFile)
        val newprops = new PropertyList()
        newprops.add(new Uid(efd.uid))
        newprops.add(new DtStamp(new DateTime()))
        newprops.add(new Summary(efd.summary))
        newprops.add(new DtStart(efd.start))
        newprops.add(new DtEnd(efd.end))
        if (efd.description != "") newprops.add(new Description(efd.description))
        if (efd.location != "") newprops.add(new Location(efd.location))
        val newvev = new VEvent(newprops)
        ical.insertNewEventInPlace(newvev)
        val outputter = new CalendarOutputter(true)
        outputter.output(ical.calendar, new FileWriter(icalFile))
      } else zenwarn(answer).run()
    }
    case DelEvent(icalFile: java.io.File, uid: String) => {
      val answer: String = zenity("allow deletion of email?").!!.stripLineEnd
      if (answer == "yes") {
        val ical = ICal.iCalFromFile(icalFile)
        val newcal = ical.deleteEvent(uid)
        val outputter = new CalendarOutputter(true)
        outputter.output(newcal, new FileWriter(icalFile))
      } else zenwarn(answer).run()
    }
  }
}

object MyWebSocketActor {
  def props(out: ActorRef, broadcast: ActorRef) = Props(new MyWebSocketActor(out, broadcast))
  case class RegisterMe ()
  case class UnRegisterMe ()
}
class MyWebSocketActor(out: ActorRef, broadcast: ActorRef) extends Actor {
  broadcast ! RegisterMe()
  val clockActor = context.actorOf(Props[ClockActor], name = "clockChild")
  clockActor ! "start"
  def receive = {
    case InternalMessage(jsn) => out ! jsn
  }
  override def postStop() = {
    clockActor ! PoisonPill
    broadcast ! UnRegisterMe()
  }
}

class ClockActor extends Actor {
  val format = new SimpleDateFormat("yyyy-MM-dd HH:mm z")
  var sa : Option[ActorRef] = None
  def sendTime(x: Option[ActorRef]) = x match {
    case None => Unit
    case Some(s) =>
      val today = Calendar.getInstance.getTime
      val timestr = format.format(today)
      val jsn: JsValue = JsObject(Seq(
        "type" -> JsString("datetime"),
        "content" ->  JsString(timestr)
      ))
      s ! InternalMessage(jsn)
  }
  def receive = {
    case "start" => {
      sa = Some(sender())
      sendTime(sa)
      context.system.scheduler.scheduleOnce(1.minute, self, "next")
    }
    case "next" => {
      sendTime(sa)
      context.system.scheduler.scheduleOnce(1.minute, self, "next")
    }
  }
}

class SocketBroadcastActor(icalFileName: String) extends Actor {
  import scala.collection.mutable.Set
  var sockActors: Set[ActorRef] = Set()
  def receive = {
    case RegisterMe() =>
      sockActors += sender()
      context.system.scheduler.scheduleOnce(
        2000.milliseconds,
        sender(),
        InternalMessage(JsObject(Seq("type" -> JsString("registered"), "content" -> JsString("registered"))))
      )
      println(sockActors)
    case UnRegisterMe() => sockActors -= sender()
      println("UnRegistered " + sender().toString())
    case m: JsValue => for (a <- sockActors) {
      a ! InternalMessage(m)
    }
    case FileEvent(fn) =>
      val jst: JsValue = JsObject(Seq("type" -> JsString("reload")))
      for (a <- sockActors) { context.system.scheduler.scheduleOnce(1000.milliseconds, a, InternalMessage(jst)) }
  }
}

