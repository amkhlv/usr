package controllers

import java.io._
import javax.inject.{Inject, Singleton}

import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.json.Json.JsValueWrapper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.io.Source
import scala.sys.process._
import scala.util.{Failure, Success}

@Singleton
class GUIQML @Inject() (val config: Configuration) extends GUI {
  val qmldir: String = config.get[String]("application.qmldir")
  def ioqml(qml: String) : ProcessBuilder = Process(
    Seq("ioqml", qmldir + "/" + qml),
    None,
    ("DISPLAY", config.get[String]("application.display")),
    ("XAUTHORITY", config.get[String]("application.xauthority"))
  )
  def json2bs(fields: (String, JsValueWrapper)*) : Array[Byte] =
    (Json.stringify(Json.obj(fields: _*)) + "\n").getBytes()
  def printStream(s: InputStream): Unit = for (ln <- Source.fromInputStream(s).getLines()) println(ln)
  override def checkMainWin(): Unit = ()
  override def warn(x: String): Unit = {
    def sendIn(stdin: OutputStream) = {stdin.write(json2bs("warning" -> x)); stdin.close()}
    def receiveOut(stdout: InputStream) = stdout.close()
    def receiveErr(stderr: InputStream) = {printStream(stderr); stderr.close()}
    ioqml("warn.qml").run(new ProcessIO(sendIn, receiveOut, receiveErr))
    ()
  }
  override def askForApproval(text: String): Future[Boolean] = {
    val response = Promise[Boolean]
    def sendIn(stdin: OutputStream) = {
      stdin.write(json2bs("question" -> text))
      stdin.flush()
      response.future.onComplete(_ => {stdin.write("stop".getBytes()); stdin.close()})
    }
    def receiveOut(stdout: InputStream) = {
      val br: BufferedReader = new BufferedReader(new InputStreamReader(stdout))
      try response.complete(Success(br.readLine() == "allow"))
      catch {case ioe: IOException => response.complete(Failure(ioe))}
      stdout.close()
    }
    def receiveErr(stderr: InputStream) = { printStream(stderr); stderr.close()}
    ioqml("deny-allow.qml").run(new ProcessIO(sendIn, receiveOut, receiveErr))
    response.future
  }
  override def askPassphrase(): Future[String] = {
    val passphrase = Promise[String]
    def sendIn(stdin: OutputStream) =
      passphrase.future.onComplete(_ => {stdin.write("stop".getBytes()); stdin.close()})
    def receiveOut(stdout: InputStream) = {
      val br: BufferedReader = new BufferedReader(new InputStreamReader(stdout))
      try {passphrase.complete(Success(br.readLine())); stdout.close()}
      catch {case ioe: IOException => passphrase.complete(Failure(ioe))}
    }
    def receiveErr(stderr: InputStream) = {printStream(stderr); stderr.close()}
    ioqml("pass-prompt.qml").run(new ProcessIO(sendIn, receiveOut, receiveErr))
    passphrase.future
  }
}
