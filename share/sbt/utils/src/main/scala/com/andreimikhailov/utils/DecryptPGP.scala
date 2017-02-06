package com.andreimikhailov.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.sys.process._
/**
  * Created by andrei on 1/7/17.
  */
class DecryptPGP(filename: String, doAskPassword: Boolean = true) {
  val system: ActorSystem = ActorSystem("DecryptionActorSystem")
  val dispatcher = system.actorOf(Props(new Dispatcher()), "DispatcherActor")
  implicit val timeout = Timeout(30.seconds)
  @tailrec private def askPassphrase() : String = {
    val r: Future[Any] = dispatcher ? AskPassword()
    val prompt = new AskPass("enter password", dispatcher)
    Await.result(r, 15.seconds) match {
      case x: String => x
      case PasswordPromptClosed() =>
        println("=== asking for the passphrase again ===")
        askPassphrase()
    }
  }
  var passphrase : String = ""
  @tailrec final def result: String = if (doAskPassword) {
    passphrase = askPassphrase()
    val is = new ByteArrayInputStream(passphrase.getBytes("UTF-8"))
    val output = new ByteArrayOutputStream()
    val r =
      ((Seq("gpg", "--batch", "--yes", "--no-tty", "--passphrase-fd", "0", "--decrypt", filename) #< is) #> output).!
    val decrypted: Option[String] = if (r>0) None else Some(output.toString("UTF-8"))
    is.close()
    output.close()
    decrypted match {
      case Some(x) => x
      case None => result
    }
  } else {
    Seq("gpg", "--batch", "--decrypt", filename).!!
  }
}
