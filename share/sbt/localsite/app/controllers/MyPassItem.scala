package controllers

import scala.xml.{Elem, Node, NodeSeq}

/**
  * Created by andrei on 1/7/17.
  */
class MyPassItem(x: xml.Node) {
  val nick : String = x.\@("nick")
  val accounts : NodeSeq = x.\("account")
  val logins : List[String] = accounts.map(acc => acc.\@("login")).toList
  val selenium : Option[Node] = x.\("selenium").toList match {
    case a::rest  =>  Some(a)
    case List() => None
  }
  val hasSelenium = selenium match {
    case Some(a) => true
    case None => false
  }
}
