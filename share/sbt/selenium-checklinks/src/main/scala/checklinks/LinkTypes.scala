package checklinks

/**
  * Created by andrei on 22/03/16.
  */

trait TypeOfLink

case class SimpleLink(url:String) extends TypeOfLink

case class LinkWithTag(url:String, tag:String) extends TypeOfLink

