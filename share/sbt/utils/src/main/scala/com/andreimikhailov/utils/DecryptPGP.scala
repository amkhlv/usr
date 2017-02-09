package com.andreimikhailov.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.sys.process._

/** PGP file decryptor
  *
  * @param filename
  * @param doAskPassword
  */
class DecryptPGP(filename: String, doAskPassword: Boolean = true) {
  implicit val ec = ExecutionContext.global
  val system: ActorSystem = ActorSystem("DecryptionActorSystem")
  val dispatcher = system.actorOf(Props(new Dispatcher()), "DispatcherActor")
  val prompt = new AskPass("enter password", dispatcher)
  implicit val timeout = Timeout(30.seconds)
  private def getPassphraseAndReentryFunction(firstTime: Boolean) : String = {
    val r: Future[Any] = dispatcher ? AskPassword()
    if (firstTime) {
      Future { blocking {prompt.main(Array())} }
    } else {
      println("=== calling back the dialogStage ===")
      prompt.bringUp()
    }
    Await.result(r, 15.seconds) match {
      case PasswordPromptClosed(None) =>
        println("=== asking for the passphrase again ===")
        getPassphraseAndReentryFunction(firstTime = false)
      case PasswordPromptClosed(Some(x)) => x
    }
  }

  @tailrec private def result1(firstTime: Boolean): String = if (doAskPassword) {
    val passphrase = getPassphraseAndReentryFunction(firstTime)
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
        result1(false)
    }
  } else {
    Seq("gpg", "--batch", "--decrypt", filename).!!
  }

  /** conents of decrypted file
    *
    * @return
    */
  def result: String = result1(firstTime = true)
}
