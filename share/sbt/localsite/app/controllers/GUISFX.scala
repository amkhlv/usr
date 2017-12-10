package controllers

import javax.inject.{Inject, Singleton}

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

@Singleton
class GUISFX @Inject() (val system: ActorSystem) extends GUI {
  val rnd = new Random()
  val mainWinActor = system.actorOf(Props(new MainWindowDispatcher), "mainwinDispatcher")
  override def checkMainWin(): Unit = {
    mainWinActor ! CheckMainWin
  }
  override def warn(x: String) = {
    mainWinActor ! x
  }
  override def askForApproval(text: String) : Future[Boolean] = {
    val token = rnd.nextString(20)
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
  override def askPassphrase() : Future[String]  = {
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
