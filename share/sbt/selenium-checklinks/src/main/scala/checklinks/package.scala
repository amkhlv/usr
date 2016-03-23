/**
  * Created by andrei on 22/03/16.
  */

import scala.io.Source

package object checklinks {
  val lines: List[String] = Source.fromFile("links.list").getLines.toList
  val links: List[TypeOfLink] = lines map {ln => {
    val splitOnHash: List[String] = ln.split("#").toList;
    if (splitOnHash.length > 2) {
      throw new Exception("more than one # in URL")
    } else {
      splitOnHash match {
        case List(u) => SimpleLink(u)
        case List(u, t) => LinkWithTag(u,t)
      }
    }
  }}
}
