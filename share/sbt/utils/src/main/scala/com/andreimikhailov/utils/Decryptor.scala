package com.andreimikhailov.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.sys.process._
import scalafx.application.JFXApp.PrimaryStage

/** PGP file decryptor
  *
  * @param filename
  * @param doAskPassword
  */
class Decryptor(
                  mainwin: PrimaryStage,
                  filename: String,
                  doAskPassword: Boolean = true,
                  gpghome: Option[String] = None
                ) {
  implicit val ec = ExecutionContext.global
  val system: ActorSystem = ActorSystem("DecryptionActorSystem")
  val dispatcher = system.actorOf(Props(new Dispatcher()), "DispatcherActor")
  val prompt = new AskPass(mainwin,"enter password", dispatcher)
  prompt.start()
  implicit val timeout = Timeout(30.seconds)
  private def getPassphraseAndReentryFunction() : String = {
    val r: Future[Any] = dispatcher ? AskPassword()
    println("=== calling back the dialogStage ===")
    prompt.bringUp()
    Await.result(r, 15.seconds) match {
      case PasswordPromptClosed(None) =>
        println("=== asking for the passphrase again ===")
        getPassphraseAndReentryFunction()
      case PasswordPromptClosed(Some(x)) => x
    }
  }
  val cmd = Seq("gpg") ++ (gpghome match { case Some(x) => Seq("--homedir", x) case None => Seq() } ) ++ Seq("--batch")

  @tailrec private def result1(): String = if (doAskPassword) {
    val passphrase = getPassphraseAndReentryFunction()
    val is = new ByteArrayInputStream(passphrase.getBytes("UTF-8"))
    val output = new ByteArrayOutputStream()
    val r =
      (((cmd ++ Seq("--yes", "--no-tty", "--passphrase-fd", "0", "--decrypt", filename)) #< is) #> output).!
    val decrypted: Option[String] = if (r>0) None else Some(output.toString("UTF-8"))
    is.close()
    output.close()
    decrypted match {
      case Some(x) => x
      case None =>
        println("=== something was wrong, perhaps wrong passphrase; executing reentry ===")
        result1()
    }
  } else {
    (cmd ++ Seq("--decrypt", filename)).!!
  }

  /** conents of decrypted file
    *
    * @return
    */
  def result: String = result1()
}
