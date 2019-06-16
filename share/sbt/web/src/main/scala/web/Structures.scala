package web

import scala.xml.{Node, NodeSeq}

class MyPassItem(x: xml.Node) {
  val nick : String = x.\@("nick")
  val accounts : NodeSeq = x.\("account")
  val logins : List[String] = accounts.map(acc => acc.\@("login")).toList
  val account : Map[String,Node] = Map((for (acc <- accounts) yield {
    acc.\@("login") -> acc
  }) :_*)
  val selenium : NodeSeq = x.\("selenium").toList match {
    case a::rest  =>  a.child
    case List() => NodeSeq.Empty
  }
}

