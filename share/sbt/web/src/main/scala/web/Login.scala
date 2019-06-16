package web

import web.utils.Decryptor

import scala.App
import scala.xml.{Node, NodeSeq}

object Login extends App {
  val usage =
    """
      |--site nick --login login --inject xml
      | ( --login and --inject are optional )
    """.stripMargin
  if (args.length == 0) println(usage)
  val arglist = args.toList
  type OptionMap = Map[Symbol, String]

  def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--site" :: value :: tail =>
          nextOption(map ++ Map('site -> value), tail)
        case "--login" :: value :: tail =>
          nextOption(map ++ Map('login -> value), tail)
        case "--inject" :: value :: tail =>
          nextOption(map ++ Map('inject -> value), tail)
        case option :: tail =>
          throw new IllegalArgumentException("Unknown option "+option)
      }
    }
  val options = nextOption(Map(),arglist)
  val sitenick = options.getOrElse('site, throw new Exception("--site is a mandatory argument"))
  println(s"-- Preparing to login to ${sitenick}")
  val x = xml.XML.loadString(Decryptor.decrypt)
  val sites = (x \\ "site").map {s => new MyPassItem(s)}.toList.filter(site =>  site.nick == sitenick)
  val injects:NodeSeq = options.get('inject) match {
    case Some(v) => xml.XML.loadString(v).child
    case None => NodeSeq.Empty
  }
  for (site <- sites) yield {
    val accs = site.accounts.filter(
      p => options.get('login) match {
        case Some(l) => p.attribute("login") match {
          case Some(n) => n.text == l
          case None => false
        }
        case None => true
      }
    )
    if (accs.isEmpty) println("-- I could not find any sites with this nickname")
    val sels:NodeSeq = site.selenium ++ injects
    if (!sels.isEmpty) {
      for ( acc <- accs ) yield {
        new LoginRobot(acc, site.selenium ++ injects) go
      }
    } else {
      println("No Selenium --- nothing to do")
    }
  }

}
