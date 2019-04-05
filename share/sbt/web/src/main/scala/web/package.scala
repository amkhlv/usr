import java.util.concurrent.TimeUnit

import com.typesafe.config.Config
import org.openqa.selenium.WebDriverException
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions}

import scala.sys.process._
import scala.collection.JavaConverters._

package object web {

  import com.typesafe.config.ConfigFactory

  val conf: Config = ConfigFactory.load()

  System.setProperty("webdriver.firefox.marionette", "true")

  def getDriver(timeout: Long): FirefoxDriver = {
    val driver = new FirefoxDriver()
    driver.manage().timeouts().implicitlyWait(timeout, TimeUnit.SECONDS)
    driver
  }

  def waitUntilUserClosesWindow(driver: FirefoxDriver) = {
    val pid = driver.getCapabilities.getCapability("moz:processID")
    val ppid : String = Process(s"ps -o ppid= $pid").!!
    while (
      try {driver.getWindowHandles.asScala.toList.length > 0} catch {
        case e: WebDriverException => false
      }
    ) { Thread.sleep(3000)}
    println(s"EXITING $ppid")
    Process(s"kill $ppid").!
  }




}
