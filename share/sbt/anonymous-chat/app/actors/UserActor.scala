package actors

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import play.api.libs.json.{JsUndefined, JsValue, Json}
import akka.actor.ActorRef
import akka.actor.Props
import scala.xml.Utility


class UserActor(uid: String, board: ActorRef, out: ActorRef) extends Actor with ActorLogging {


  override def preStart() = {
    BoardActor() ! Subscribe
  }

  def receive = LoggingReceive {
    case Message(muid, s) if sender == board => {
      val js = Json.obj(
        "type" -> "message",
        "uid" -> muid,
        //"color" -> controllers.Application.colors(muid.toInt % controllers.Application.ncolors),
        "color" -> controllers.Chat.color(muid),
        "msg" -> s
      )
      out ! js
    }
    case js: JsValue => {
      (js \ "token") match {
        case e: JsUndefined => ()
        case v: JsValue => v.validate[String] map {
          Utility.escape(_)
        } map {
          t => {
            if (controllers.Application.toks.contains(t)) {
              controllers.Application.trustedActors = controllers.Application.trustedActors + this.self
            }
            else {
              log.warning(" === CSRF attempt? === ")
            }
            println(controllers.Application.toks)
          }
        }
        case _ => throw new RuntimeException("Unknown js.token")
      }
      (js \ "msg") match {
        case e: JsUndefined => ()
        case v: JsValue => v.validate[String] map {
          Utility.escape(_)
        } map {
          board ! Message(uid, _)
        }
        case _ => throw new RuntimeException("Unknown js.msg")
      }
    }
    case other => log.error("unhandled: " + other)
  }
}

object UserActor {
  def props(uid: String)(out: ActorRef) = Props(new UserActor(uid, BoardActor(), out))
}
