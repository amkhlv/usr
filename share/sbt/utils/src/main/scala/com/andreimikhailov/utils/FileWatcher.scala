package com.andreimikhailov.utils

/**
  * Created by andrei on 25/12/16.
  */
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{FileSystems, WatchService, Path => JNPath}

import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConversions._
import scala.concurrent.{Future, Promise, blocking}


case class Stopped() extends RuntimeException

/**
  * Watches files in a given directory for changes and sends interesting filenames
  * to the specifed actor's mailbox
  * @constructor  create a new [[FileWatcher]]
  * @param directoryPath  path to watched directory
  * @param reportTo  actor to report to
  * @param isInteresting  test on filename to determine if an event is interesting
  * @param knockoutInterval  for how long (in milliseconds) to stop processing events after detecting an interesting one
  */
class FileWatcher(
                   directoryPath: JNPath,
                   reportTo: ActorRef,
                   isInteresting: (String => Boolean) = (x => true),
                   knockoutInterval: Long = 0
                 ) {
  private val watcher: WatchService = FileSystems.getDefault().newWatchService();
  private var key = directoryPath.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)
  private val prms = Promise[Unit]
  def start: Future[Unit] = prms.future
  def stop: Unit = prms.tryFailure(Stopped())
  prms.tryCompleteWith(Future {blocking {
    var lastInterestingTime = 0L
    while (!prms.isCompleted) {
      val key = watcher.take()
      for (event <- key.pollEvents) {
        val kind = event.kind()
        if (kind != OVERFLOW) {
          val relativePath = event.context().asInstanceOf[JNPath]
          val fname = relativePath.getFileName.toString
          if (isInteresting(fname)) {
            if (
              (knockoutInterval == 0) || {
                val nowms = System.currentTimeMillis()
                if ((nowms - lastInterestingTime) > knockoutInterval) {
                  lastInterestingTime = nowms
                  true
                } else false
              }
            ) { reportTo ! FileEvent(relativePath.getFileName.toString) }
          }
        }
      }
      key.reset()
    }}})

}
