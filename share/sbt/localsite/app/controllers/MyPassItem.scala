package controllers

import scala.xml.{Node, NodeSeq}

/**
  * Created by andrei on 1/7/17.
  */
class MyPassItem(x: xml.Node) {
  val nick : String = x.\@("nick")
  val accounts : NodeSeq = x.\("account")
  val logins : List[String] = accounts.map(acc => acc.\@("login")).toList
  val password : Map[String,String] = Map((for (acc <- accounts ) yield {
    acc.\@("login") -> acc.\("password").text
  }) :_*)
  val selenium : Option[Node] = x.\("selenium").toList match {
    case a::rest  =>  Some(a)
    case List() => None
  }
  val hasSelenium = selenium match {
    case Some(a) => true
    case None => false
  }
  val canShow : Boolean = !(x.\("canShow").isEmpty)
}
