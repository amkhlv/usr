package web.utils

import scalaj.http._
import net.liftweb.json._
import web.conf

object AntiCaptcha {

  implicit val formats = DefaultFormats

  def defeat(base64string: String): Option[String] = {
    val captchaTaskJSON = Http("https://api.anti-captcha.com/createTask").postData(
      s"""
         |{
         | "clientKey" : "${conf().getString("antiCaptchaClientKey")}",
         | "task" : {
         |           "type" : "ImageToTextTask",
         |           "body" : "$base64string"
         |          }
         |}
    """.stripMargin
    ).asString.body
    val json = parse(captchaTaskJSON)
    val taskid = json.extract[AntiCaptchaTask].taskId
    println("=== captchaTask ===")
    println(captchaTaskJSON)
    println("===================")
    Thread.sleep(30000)
    val solvedCaptchaJSON = Http("https://api.anti-captcha.com/getTaskResult").postData(
      s"""
         |{
         |"clientKey" : "${conf().getString("antiCaptchaClientKey")}",
         |"taskId" : $taskid
         |}
     """.stripMargin
    ).asString.body
    val jsonOfResult = parse(solvedCaptchaJSON)
    val solution = jsonOfResult.extract[CaptchaSolved].solution
    Some(solution.text)

  }

}

case class AntiCaptchaTask(errorId: String, taskId: String)
case class CaptchaSolution(text: String, url: String)
case class CaptchaSolved(
                          errorId: String,
                          status: String,
                          solution: CaptchaSolution,
                          cost: String,
                          ip: String,
                          createTime: String,
                          endTime: String,
                          solveCount: String
                        )
