package web.sites

import java.net.URLEncoder
import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import org.openqa.selenium.{By, JavascriptExecutor, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.support.ui.Select
import web.conf

import scala.App
import scala.collection.mutable
import scala.xml._

class InspireList(driver: FirefoxDriver) {

  private val bibitems = conf().getStringList("bibitems").asScala

  val publications = <publications>
    {bibitems.map(
      item => {
        val url = s"http://inspirehep.net/search?ln=en&p=${URLEncoder.encode(item, "UTF-8")}"
        driver.get(url)
        val record = driver.findElementByClassName("record_body")
        val ttl = driver.findElementByClassName("titlelink")
        val authors = driver.findElementsByClassName("authorlink").asScala.toList
        val details = driver.findElementByXPath("/html/body/div[3]/div/table[1]/tbody/tr/td[2]/div[1]/small")
        val stats = driver.findElementsByClassName("moreinfo").asScala
        val ss = stats.filter(_.getTagName == "a").filter(_.getText contains "Cited by")
        <article>
          <title>{ttl.getText}</title>
          <authors>{authors.map(
            author =>
              <a href={author.getAttribute("href")}>{author.getText}</a><nbsp/>
          )}</authors>
          <data>{details.getText}</data>
          <stats>{ss.map(stat => <a href={stat.getAttribute("href")}>{stat.getText}</a>)}</stats>
          <inSpire><a href={url}>{item}</a></inSpire>
        </article>
      }
    )}
  </publications>

//  val pp = new PrettyPrinter(100, 2)
//  println(pp.format(publications))
}

