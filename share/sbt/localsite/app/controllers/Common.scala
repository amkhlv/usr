package controllers
import javax.inject._

import akka.actor._
import play.api.Configuration
//@ImplementedBy(classOf[MyCommon])
//above is commented because see app/Module.scala
trait Common {
  val system: ActorSystem
  val icalFile: java.io.File
  val icalDir:  java.nio.file.Path
  val icalFileName: String
  val googleCalendar: String
  val broadcastActor: ActorRef
  val gui: GUI
}
/**
 * This class is a concrete implementation of my Common trait.
 * It is configured for Guice dependency injection in the app/Module.class
 * This class has a `Singleton` annotation because we need to make
 * sure we only use one counter per application. 
 */
@Singleton
class MyCommon @Inject() (val config: Configuration, val gui: GUI, val system: ActorSystem) extends Common {
  val icalFile  = new java.io.File(config.get[String]("application.ics"))
  val icalDir = icalFile.getParentFile.toPath
  val icalFileName : String = icalFile.toPath.getFileName.toString
  val googleCalendar = config.get[String](path = "application.googleCalendar")
  val broadcastActor = system.actorOf(Props(new SocketBroadcastActor(icalFileName)), "broadcastActor")
}
