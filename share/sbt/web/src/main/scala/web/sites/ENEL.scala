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

  val w = new WebDriverWait(driver,3600)
  w.until(ExpectedConditions.titleContains("Enel"))

  Thread.sleep(2000)

  def firstPageClickFails(): Boolean = {
    driver.findElement(By.id("cpfcnpj")).sendKeys(conf.getString("CPF"))
    driver.findElement(By.id("anlage")).sendKeys(conf.getString("inst"))
    val ldnBtn = driver.findElement(By.id("btnLogin"))
    w.until(ExpectedConditions.elementToBeClickable(ldnBtn))
    ldnBtn.click()
    false
  }

  def secondPageClickFails(): Boolean ={
    val yesBtn = driver.findElement(By.xpath("/html/body/div[1]/div/md-content/div/div[1]/a/button"))
    yesBtn.click()
    false
  }

  def thirdPageClickFails(): Boolean = {
    val homeBtn = driver.findElement(By.xpath("/html/body/div[1]/div/md-sidenav/div/div[1]/div/div[2]/button[1]"))
    homeBtn.click()
    false
  }

  def tryclick(clickFails: () => Boolean): Unit  = {
    while (try {
      clickFails()
    } catch {
      case e: ElementClickInterceptedException => {
        true
      }
    }
    ) Thread.sleep(1000)
  }

  Thread.sleep(2000)

  tryclick(firstPageClickFails)

  Thread.sleep(2000)

  tryclick(secondPageClickFails)

  Thread.sleep(2000)

  tryclick(thirdPageClickFails)







  waitUntilUserClosesWindow(driver)


}
