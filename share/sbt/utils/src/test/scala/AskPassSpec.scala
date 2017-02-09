/**
  * Created by andrei on 1/7/17.
  */

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.andreimikhailov.utils._
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}


class AskPassSpec extends FlatSpec with Matchers  {
  implicit val ec = ExecutionContext.global
  val system:ActorSystem =  ActorSystem("TestAskPassSystem")
  val inspector = system.actorOf(Props(new Dispatcher()),"TestActor")
  implicit val timeout = Timeout(15.seconds)
  val r : Future[Any] = inspector ? AskPassword()
  val ap = new AskPass("password for testing", inspector)
  Future{ blocking {ap.main(Array())}}
  val p = Await.result(r, 15.seconds) match {
    case PasswordPromptClosed(Some(x)) => x
    case PasswordPromptClosed(None) =>
      println("=== atmospheric re-entry ! ===")
      val r1 : Future[Any] =  inspector ? AskPassword()
      ap.bringUp()
      Await.result(r1, 15.seconds) match {
        case PasswordPromptClosed(Some(x1)) => x1
        case PasswordPromptClosed(None) => "PASSWORD NOT ENTERED ON SND ATTEMPT"
      }
  }
  println("=== PASSWORD was: -->" + p + "<--")
  "AskPass" should "ask password" in {

  }

}
