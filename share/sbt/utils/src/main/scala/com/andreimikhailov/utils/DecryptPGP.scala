package com.andreimikhailov.utils

import java.io.ByteArrayInputStream
import java.nio.file.Path

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.sys.process._
/**
  * Created by andrei on 1/7/17.
  */
class DecryptPGP(filename: String, doAskPassword: Boolean = true) {
  lazy val passphrase : String = {
    val system: ActorSystem = ActorSystem("DecryptionActorSystem")
    val dispatcher = system.actorOf(Props(new Dispatcher()), "TestActor")
    implicit val timeout = Timeout(15.seconds)
    val r: Future[Any] = dispatcher ? AskPassword()
    val prompt = new AskPass("enter password", dispatcher)
    Await.result(r, 15.seconds) match {
      case x: String => x
    }
  }
  def result = if (doAskPassword) {
    val is = new ByteArrayInputStream(passphrase.getBytes("UTF-8"))
    (Seq("gpg", "--batch", "--passphrase-fd", "0", "--decrypt", filename) #< is).!!
  } else {
    Seq("gpg", "--batch", "--decrypt", filename)   .!!
  }
}
