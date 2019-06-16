package web.sites


import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import org.openqa.selenium.{By, JavascriptExecutor, WebDriverException, WebElement}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}
import org.openqa.selenium.support.ui.{ExpectedCondition, ExpectedConditions, Select, WebDriverWait}
import web.{conf, getDriver, waitUntilUserClosesWindow}

object SubCRNM extends App {
  val driver = getDriver(20)
  driver.get("https://servicos.dpf.gov.br/sismigra-internet/faces/publico/verificarProtocolo/verificarProtocolo.seam?cid=200")

  val w = new WebDriverWait(driver,3600)
  w.until(ExpectedConditions.titleContains("SISMIGRA"))

  driver.findElementById("form-validar-protocolo:txt-protocolo").sendKeys(web.conf.getString("requerimento"))

  driver.findElementById("form-validar-protocolo:txt-codigo-controle").sendKeys(web.conf.getString("codigoControle"))

  driver.findElementById("form-validar-protocolo:j_id47").click()

  waitUntilUserClosesWindow(driver)



}