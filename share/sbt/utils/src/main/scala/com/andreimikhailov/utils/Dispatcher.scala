package com.andreimikhailov.utils

import akka.actor.{Actor, ActorRef}

/**
  * Created by andrei on 1/7/17.
  */
class Dispatcher extends  Actor {
  val sendersWaitingForPassword = scala.collection.mutable.SortedSet[ActorRef]()

  override def receive = {
    case AskPassword() =>
      sendersWaitingForPassword += sender()
    case Password(p) =>
      for ( s <- sendersWaitingForPassword) { s ! p }
    case _ => throw  new Exception("wrong message to ASK PASS actor")
  }

}
