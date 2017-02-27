package controllers

/**
  * Created by andrei on 1/7/17.
  */

import java.util.concurrent.TimeUnit

import org.openqa.selenium.{By, WebDriver}
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxProfile}

import scala.xml.{Node, NodeSeq};



class MySelenium (account: Node, sel: Node, profile: Either[FirefoxProfile,ChromeOptions])  {
  val driver : WebDriver = profile match {
    case Left(ff) => new FirefoxDriver(ff)
    case Right(co) => new ChromeDriver(co)
  }
  /*val driver: WebDriver = profile match {
    case Some(prfl) => new FirefoxDriver(prfl)
    case None => new FirefoxDriver()
  }*/
  driver.manage().timeouts().implicitlyWait(30, TimeUnit.SECONDS)
  val steps: NodeSeq = sel.\("_")
  def go = {
    for (step <- steps) {
      println("STEP ----- " + step.toString())
      step.label match {
        case "goto" => driver.get(step.text)
        case "elt" => {
          val elt = {
            val f = step.\@("find")
            step.\@("by") match {
              case "linkText" => driver.findElement(By.linkText(f))
              case "id" => driver.findElement(By.id(f))
              case "name" => driver.findElement(By.name(f))
              case "class" => driver.findElement(By.className(f))
            }
          }
          for (action <- step.\("_")) {
            action.label match {
              case "click" => elt.click()
              case "login" => elt.sendKeys(account.\@("login"))
              case "passwd" => elt.sendKeys(account.\("password").text)
            }
          }
        }
        case "sleep" => Thread.sleep(Integer.parseInt(step.\@("t")))
        case "defaultContent" => driver.switchTo().defaultContent()
        case "frame" => driver.switchTo().frame(step.text)
      }
    }
  }
}
