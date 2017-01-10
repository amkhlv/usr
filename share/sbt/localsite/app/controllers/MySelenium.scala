package controllers

/**
  * Created by andrei on 1/7/17.
  */

import java.util.concurrent.TimeUnit

import org.openqa.selenium.By
import org.openqa.selenium.WebDriver
import org.openqa.selenium.WebElement
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.support.ui.ExpectedCondition
import org.openqa.selenium.support.ui.WebDriverWait

import scala.xml.{Node, NodeSeq};



class MySelenium(account: Node, sel: Node) {
  val driver: WebDriver = new FirefoxDriver()
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
        case "sleep" => Thread.sleep(5000)
      }
    }
  }
}