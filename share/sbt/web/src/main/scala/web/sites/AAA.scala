package web.sites

import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import org.openqa.selenium.{By, JavascriptExecutor, WebDriverException, WebElement}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}
import org.openqa.selenium.support.ui.{ExpectedCondition, ExpectedConditions, Select, WebDriverWait}
import web.{conf, getDriver, waitUntilUserClosesWindow}

object AAA extends App {
  val driver = getDriver(20)
  driver.get("https://www.google.com")

  val w = new WebDriverWait(driver,3600)
  w.until(ExpectedConditions.titleContains("Google"))

  waitUntilUserClosesWindow(driver)



}
