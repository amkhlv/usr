package web.sites

import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import org.openqa.selenium.{By, JavascriptExecutor, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.support.ui.Select
import web.conf
import java.net.URLEncoder
import scala.App
import scala.collection.mutable

import scala.xml._

class InspireCitations(driver: FirefoxDriver) {
  driver.get(s"http://inspirehep.net/author/profile/${conf.getString("author")}")
  driver.manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS)

  private val citations = driver.findElement(By.xpath("""//*[@id="citations"]"""))


  private val x = citations.findElements(By.tagName("table")).asScala
  val pubstats = <rows>{
    x(0).findElements(By.tagName("tr")).asScala.filter(
      el => el.findElements(By.tagName("th")).asScala.length == 1
    ).map(
      el => {
        val th = el.findElement(By.tagName("th"))
        val tds = el.findElements(By.tagName("td")).asScala
        <row Item={th.getText} Citeable={tds(0).getText} Published={tds(1).getText}></row>
      }
    )
    }</rows>

//  val pp = new PrettyPrinter(100,2)
//  println(pp.format(pubstats))

}

