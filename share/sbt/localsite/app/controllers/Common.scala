package controllers
import javax.inject._

import akka.actor._
import play.api.Configuration

import scala.util.Random
//@ImplementedBy(classOf[MyCommon])
//above is commented because see app/Module.scala
trait Common {
  val rnd: Random
  val system: ActorSystem
  val icalFile: java.io.File
  val icalDir:  java.nio.file.Path
  val icalFileName: String
  val broadcastActor: ActorRef
  val launchActor: ActorRef
  val mainWinActor: ActorRef
}
/**
 * This class is a concrete implementation of my Common trait.
 * It is configured for Guice dependency injection in the app/Module.class
 * This class has a `Singleton` annotation because we need to make
 * sure we only use one counter per application. 
 */
@Singleton
class MyCommon @Inject() (config: Configuration) extends Common {
  val rnd = new Random()
  val system = ActorSystem("MainActorSystem")
  val icalFile  = new java.io.File(config.get[String]("application.ics"))
  val icalDir = icalFile.getParentFile.toPath
  val icalFileName : String = icalFile.toPath.getFileName.toString
  val broadcastActor = system.actorOf(Props(new SocketBroadcastActor(icalFileName)), "broadcastActor")
  val launchActor = system.actorOf(LaunchActor.props(broadcastActor), "launchActor")
  val mainWinActor = system.actorOf(Props(new MainWindowDispatcher), "mainwinDispatcher")
}
