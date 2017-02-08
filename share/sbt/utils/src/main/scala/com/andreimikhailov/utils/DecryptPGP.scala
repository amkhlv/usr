package com.andreimikhailov.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.sys.process._
/**
  * Created by andrei on 1/7/17.
  */
class DecryptPGP(filename: String, doAskPassword: Boolean = true) {
  implicit val ec = ExecutionContext.global
  val system: ActorSystem = ActorSystem("DecryptionActorSystem")
  val dispatcher = system.actorOf(Props(new Dispatcher()), "DispatcherActor")
  val prompt = new NestedAskPass("enter password", dispatcher)
  implicit val timeout = Timeout(30.seconds)
  private def getPassphraseAndReentryFunction(ofun: Option[() => Unit]) : (String,  () => Unit) = {
    val r: Future[Any] = dispatcher ? AskPassword()
    ofun match {
      case None => Future { blocking {(new prompt.PassGui).main(Array())}}
      case Some(f) =>
        println("=== calling back the dialogStage ===")
        f()
    }
    Await.result(r, 15.seconds) match {
      case PasswordPromptClosed(None, f) =>
        println("=== asking for the passphrase again ===")
        getPassphraseAndReentryFunction(Some(f))
      case PasswordPromptClosed(Some(x), f) => (x, f)
    }
  }

  @tailrec final def result1(opReentryFun: Option[() => Unit]): String = if (doAskPassword) {
    val (passphrase, reentryFun) = getPassphraseAndReentryFunction(opReentryFun)
    val is = new ByteArrayInputStream(passphrase.getBytes("UTF-8"))
    val output = new ByteArrayOutputStream()
    val r =
      ((Seq("gpg", "--batch", "--yes", "--no-tty", "--passphrase-fd", "0", "--decrypt", filename) #< is) #> output).!
    val decrypted: Option[String] = if (r>0) None else Some(output.toString("UTF-8"))
    is.close()
    output.close()
    decrypted match {
      case Some(x) => x
      case None =>
        println("=== something was wrong, perhaps wrong passphrase; executing reentry ===")
        result1(Some(reentryFun))
    }
  } else {
    Seq("gpg", "--batch", "--decrypt", filename).!!
  }

  def result: String = result1(None)
}
