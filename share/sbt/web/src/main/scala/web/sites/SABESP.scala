package web.sites

import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import org.openqa.selenium.{By, JavascriptExecutor, WebDriverException, WebElement}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}
import org.openqa.selenium.support.ui.{ExpectedCondition, ExpectedConditions, Select, WebDriverWait}
import web.{conf, getDriver, waitUntilUserClosesWindow}

object SABESP extends App {
  val driver = getDriver(20)

  driver.get("https://www9.sabesp.com.br/agenciavirtual/pages/template/siteexterno.iface?idFuncao=19")

  val w = new WebDriverWait(driver,3600)
  w.until(ExpectedConditions.titleContains("Sabesp"))

  Thread.sleep(2000)

  driver.findElement(By.id("frmhome:rgi1")).sendKeys(conf().getString("RGI"))

  Thread.sleep(2000)

  driver.findElement(By.id("frmhome:j_id172")).click()

  Thread.sleep(2000)

  driver.findElement(By.id("frmhome:j_id206")).click()

  waitUntilUserClosesWindow(driver)







}