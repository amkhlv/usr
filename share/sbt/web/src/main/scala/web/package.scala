import java.util.concurrent.TimeUnit

import com.typesafe.config.{Config, ConfigException}
import org.openqa.selenium.{WebDriver, WebDriverException}
import org.openqa.selenium.firefox.{FirefoxDriver, FirefoxOptions, FirefoxProfile}
import org.openqa.selenium.remote.{CapabilityType, DesiredCapabilities}

import scala.sys.process._
import scala.collection.JavaConverters._

package object web {

  import com.typesafe.config.ConfigFactory

  def conf: () => Config = ConfigFactory.load

  val rnd: scala.util.Random = new scala.util.Random()

  System.setProperty(FirefoxDriver.SystemProperty.DRIVER_USE_MARIONETTE, "true")
  System.setProperty(FirefoxDriver.SystemProperty.BROWSER_PROFILE, "Robot")

  def getDriver(timeout: Long): FirefoxDriver = {
    val ffOptions = new FirefoxOptions()
    try {
      val h = conf().getInt("height")
      ffOptions.addArguments(s"--height=$h")
    } catch {
      case _ : ConfigException.Missing => ()
    }
    try {
      val w = conf().getInt("width")
      ffOptions.addArguments(s"--width=$w")
    } catch {
      case _ : ConfigException.Missing => ()
    }
    val ffProfilePath = new java.io.File("/home/andrei/.mozilla/firefox/Robot")
    val ffProfile = new FirefoxProfile(ffProfilePath)
    ffProfile.setAcceptUntrustedCertificates(false)
    ffOptions.setProfile(ffProfile)
    ffOptions.setAcceptInsecureCerts(false)
    val driver = new FirefoxDriver(ffOptions)
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
