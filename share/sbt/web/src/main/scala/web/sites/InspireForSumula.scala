package web.sites

import java.util.concurrent.TimeUnit

import org.openqa.selenium.firefox.FirefoxDriver

import scala.App
import scala.xml
import scala.xml.{Elem, PrettyPrinter}


object InspireForSumula extends App {
  val driver = new FirefoxDriver()
  driver.manage().timeouts().implicitlyWait(20, TimeUnit.SECONDS)
  val publist = new InspireList(driver).publications
  val pubstats = new InspireCitations(driver).pubstats
  val pp = new PrettyPrinter(100,2)
  println(pp.format(
   <root>
     {publist}
     {pubstats}
   </root>
  ))

}
