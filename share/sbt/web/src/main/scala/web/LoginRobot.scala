package web

import org.openqa.selenium.By

import scala.xml.{Node, NodeSeq}
class LoginRobot (account: Node, sel: NodeSeq)  {
  println("-- Will do:")
  for (n <- sel) yield {
    println(n.toString())
  }
  val driver = getDriver(60)
  def go = {
    for (step <- sel) {
      println("STEP ----- " + step.toString())
      step.label match {
        case "goto" => {
          println("-- navigating to: " + step.text)
          driver.get(step.text)
        }
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
        case  "#PCDATA" => ()
        case x => {
          println("-- unknown step: ")
          println("-- xmlType: " + step.xmlType())
          println("-- toString: " + step.toString())
          println("-- label: " + step.label)
          println("-- text: -->" + step.text +"<--")
        }
      }
    }
    web.waitUntilUserClosesWindow(driver)
  }
}


