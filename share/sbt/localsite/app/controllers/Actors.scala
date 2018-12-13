package controllers

/**
  * Created by andrei on 12/13/16.
  */

import java.text.SimpleDateFormat
import java.time._
import java.time.format._

import akka.actor._
import com.andreimikhailov.utils.FileEvent
import controllers.MyWebSocketActor.{RegisterMe, UnRegisterMe}
import play.api.libs.json.{JsObject, JsString, JsValue}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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
  var sa : Option[ActorRef] = None
  def sendTime(x: Option[ActorRef]) = x match {
    case None => Unit
    case Some(s) =>
      val today = ZonedDateTime.now()
      val formatter = DateTimeFormatter.RFC_1123_DATE_TIME
      val jsn: JsValue = JsObject(Seq(
        "type" -> JsString("datetime"),
        "content" ->  JsString(today.format(formatter))
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

