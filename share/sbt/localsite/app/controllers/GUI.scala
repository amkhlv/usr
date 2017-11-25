package controllers

import akka.actor.ActorRef
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global

object GUI {
  def askForApproval(text: String, token: String, mainWinActor: ActorRef) : Future[Boolean] = {
    implicit val timeout: Timeout = 20.seconds;
    {
      for {
        r <- mainWinActor ? ApprovalRequest("allow ICal update?", token)
      } yield {
        r match {
          case ApprovalResponse(true, t) => t == token
          case _ => false
        }
      }
    }
  }
  def askPassphrase(mainWinActor: ActorRef) : Future[String]  = {
    implicit val timeout: Timeout = 60.seconds;
    {
      for {
        r <- mainWinActor ? AskPass()
      } yield {
        r match {
          case pp: String => pp
          case _ => ""
        }
      }
    }
  }

}
