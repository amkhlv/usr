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
    case PasswordPromptClosed(ox) =>
      for ( s <- sendersWaitingForPassword) {
        println("=== password prompt notifying " + s.toString())
        s ! PasswordPromptClosed(ox)
        sendersWaitingForPassword.-=(s)
      }
    case _ => throw  new Exception("wrong message to ASK PASS actor")
  }

}
