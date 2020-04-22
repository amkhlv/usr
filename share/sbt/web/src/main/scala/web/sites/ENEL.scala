package web.sites

import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import org.openqa.selenium.{By, ElementClickInterceptedException, JavascriptExecutor, WebDriverException, WebElement}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}
import org.openqa.selenium.support.ui.{ExpectedCondition, ExpectedConditions, Select, WebDriverWait}
import web.{conf, getDriver, waitUntilUserClosesWindow}


object ENEL extends App {
  val driver = getDriver(20)
  driver.get("https://portalhome.eneldistribuicaosp.com.br/#/login")

  val w = new WebDriverWait(driver,30)
  w.until(ExpectedConditions.titleContains("Enel"))

  Thread.sleep(2000)


  val locCPF=By.id("cpfcnpj")
  val locANLAGE=By.id("anlage")
  val locLogin=By.id("btnLogin")
  w.until(ExpectedConditions.presenceOfElementLocated(locCPF))
  driver.findElement(locCPF).sendKeys(conf().getString("CPF"))
  w.until(ExpectedConditions.presenceOfElementLocated(locANLAGE))
  driver.findElement(locANLAGE).sendKeys(conf().getString("inst"))
  w.until(ExpectedConditions.presenceOfElementLocated(locLogin))
  val lgnBtn = driver.findElement(locLogin)
  w.until(ExpectedConditions.elementToBeClickable(lgnBtn))
  Thread.sleep(3000)
  lgnBtn.click()

  w.until(ExpectedConditions.presenceOfElementLocated(By.id("question1")))
  Thread.sleep(3000)
  driver.findElement(By.id("question1")).sendKeys(conf().getString("tel"))

  Thread.sleep(1000)

  w.until(ExpectedConditions.elementToBeClickable(By.id("avancar2")))
  driver.findElement(By.id("avancar2")).click()


  val locYesBtn = By.xpath("/html/body/div[1]/div/md-content/div/div[1]/a/button")
  w.until(ExpectedConditions.presenceOfElementLocated(locYesBtn))
  w.until(ExpectedConditions.elementToBeClickable(locYesBtn))
  driver.findElement(locYesBtn).click()

  val locHomeBtn = By.xpath("/html/body/div[1]/div/md-sidenav/div/div[1]/div/div[2]/button[1]")
  w.until(ExpectedConditions.presenceOfElementLocated(locHomeBtn))
  w.until(ExpectedConditions.elementToBeClickable(locHomeBtn))
  driver.findElement(locHomeBtn).click()

  waitUntilUserClosesWindow(driver)


}
