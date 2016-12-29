import java.io.{BufferedWriter, FileWriter}

import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.scalatest._
import com.andreimikhailov.utils._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

/**
  * Created by andrei on 12/26/16.
  */
class FileWatcherSpec extends FlatSpec with Matchers {
  val system:ActorSystem =  ActorSystem("TestSystem")
  case class RequestFileList()
  class MyTestActor() extends Actor {
    val files = scala.collection.mutable.SortedSet[String]()
    def receive = {
      case FileEvent(x)  =>
        println("ACTOR: " + x)
        files += x
      case RequestFileList => sender() ! files
      case x => throw  new Exception("wrong message to actor")
    }
  }
  val reporter = system.actorOf(Props(new MyTestActor()),"TestActor")
  val curDir = (new java.io.File(".")).toPath
  println(curDir.toString)
  val fw = new FileWatcher((new java.io.File(".")).toPath,reporter)
  "FileWatcher" should "exit clean" in {
    implicit val timeout = Timeout(10 seconds)
    fw.start.onComplete {
      case Failure(Stopped()) => println("OK: FileWatcher stopped")
      case _ => println("ERROR in file watcher")
    }
    println("started Future in FileWatcher")
    Thread.sleep(1000)
    val testFile1 = new java.io.File("eraseme1")
    val bw1 = new BufferedWriter(new FileWriter(testFile1))
    bw1.write("hi-1")
    bw1.close()
    Thread.sleep(1000)
    val testFile2 = new java.io.File("eraseme2")
    val bw2 = new BufferedWriter(new FileWriter(testFile2))
    bw2.write("hi-2")
    bw2.close()
    Thread.sleep(1000)
    fw.stop
    val ftr = reporter ? RequestFileList
    val result = Await.ready(ftr, Duration.Inf).value.get
    result should (equal(Success(scala.collection.mutable.SortedSet[String]("eraseme1","eraseme2"))))
  }
}
