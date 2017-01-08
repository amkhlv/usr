package controllers

/**
  * Created by andrei on 12/13/16.
  */

import java.util.Calendar

import akka.actor._
import com.andreimikhailov.utils.FileEvent
import controllers.MyWebSocketActor.{RegisterMe, UnRegisterMe}
import play.api.libs.json.{JsObject, JsString, JsValue}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


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
    case jsn: JsValue => out ! jsn
  }
  override def postStop() = {
    clockActor ! PoisonPill
    broadcast ! UnRegisterMe()
  }
}

class ClockActor extends Actor {
  val frequency = 1000
  var sa : Option[ActorRef] = None
  def sendTime(x: Option[ActorRef]) = x match {
    case None => Unit
    case Some(s) =>
      val today = Calendar.getInstance.getTime
      val timestr = today.toString
      val jsn: JsValue = JsObject(Seq(
        "type" -> JsString("datetime"),
        "content" ->  JsString(timestr)
      ))
      s ! jsn
  }
  def receive = {
    case "start" => {
      sa = Some(sender())
      sendTime(sa)
      Thread.sleep(frequency)
      self ! "next"
    }
    case "next" => {
      sendTime(sa)
      Thread.sleep(frequency)
      self ! "next"
    }
  }
}

class SocketBroadcastActor(icalFileName: String) extends Actor {
  import scala.collection.mutable.Set
  var sockActors: Set[ActorRef] = Set()
  def receive = {
    case RegisterMe() => sockActors += sender()
      println(sockActors)
    case UnRegisterMe() => sockActors -= sender()
      println("UnRegistered " + sender().toString())
    case m: JsValue => for (a <- sockActors) {
      a ! m
    }
    case FileEvent(fn) =>
      val jst: JsValue = JsObject(Seq("type" -> JsString("reload")))
      for (a <- sockActors) { context.system.scheduler.scheduleOnce(1000.milliseconds, a, jst) }
  }
}

