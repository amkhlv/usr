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
      for ( s <- sendersWaitingForPassword) {
        s ! p
        sendersWaitingForPassword.-=(s)
      }
    case PasswordPromptClosed() =>
      println("=== Password Prompt Closed ===")
      for ( s <- sendersWaitingForPassword) {
        println("=== notifying " + s.toString())
        s ! PasswordPromptClosed()
        sendersWaitingForPassword.-=(s)
      }
    case _ => throw  new Exception("wrong message to ASK PASS actor")
  }

}
