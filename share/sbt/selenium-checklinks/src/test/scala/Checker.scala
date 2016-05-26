import checklinks._
import org.scalatest.selenium.HtmlUnit
import org.scalatest.{FlatSpec, ShouldMatchers}


/**
  * Created by andrei on 21/03/16.
  */
class Checker extends FlatSpec with ShouldMatchers with HtmlUnit {
  //implicit val webDriver: WebDriver = new HtmlUnitDriver
  //webDriver.manage().timeouts().pageLoadTimeout(8, TimeUnit.SECONDS)

  for (lnk <- checklinks.links) {
    println(lnk)
    lnk match {
      case SimpleLink(url) => {
        go to (url)
        val bodies:Iterator[Element] = findAll(tagName("body"))
        bodies.next().attribute("id") should be (Some("scribble-racket-lang-org"))
        //bodies.length should be (1)
      }
      case LinkWithTag(url, tag) => {
        println("Going to visit: " + url + "#" + tag)
        go to (url)
        val anchors: Iterator[Element] = findAll(tagName("a"));
        val names = anchors.map(a => a.attribute("name")).collect { case Some(n) => n };
        names.filter(_ == tag).toList should be (List(tag))
      }
    }
  }



  //println(findAll(name("(part._.Technical_links)")))
}

