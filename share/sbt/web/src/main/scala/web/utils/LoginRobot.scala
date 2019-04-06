package web.utils

import java.util.concurrent.TimeUnit

import org.openqa.selenium.By
import org.openqa.selenium.firefox.FirefoxDriver
import web.getDriver

import scala.xml.{Node, NodeSeq}
class LoginRobot (account: Node, sel: Node)  {
  val driver = getDriver(60)
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
              case "xpath" => driver.findElement(By.xpath(f))
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
    web.waitUntilUserClosesWindow(driver)
  }
}


