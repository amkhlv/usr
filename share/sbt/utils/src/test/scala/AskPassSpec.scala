/**
  * Created by andrei on 1/7/17.
  */

import org.scalatest._
import com.andreimikhailov.utils._
import akka.actor._
import akka.actor.{Actor, ActorSystem, Props, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
class AskPassSpec extends FlatSpec with Matchers  {
  val system:ActorSystem =  ActorSystem("TestAskPassSystem")
  val inspector = system.actorOf(Props(new Dispatcher()),"TestActor")
  implicit val timeout = Timeout(15.seconds)
  val r : Future[Any] = inspector ? AskPassword()
  val ap = new AskPass("password for testing", inspector)
  println("=== PASSWORD was: -->" + Await.result(r, 15.seconds) + "<--")
  "AskPass" should "ask password" in {

  }

}
