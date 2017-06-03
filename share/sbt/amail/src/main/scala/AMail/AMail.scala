package AMail

import java.io.{File, FileInputStream}
import java.time.Instant

import com.typesafe.config.{Config, ConfigFactory}
import org.apache.james.mime4j.parser.MimeStreamParser
import org.apache.james.mime4j.stream.MimeConfig
import org.sqlite.SQLiteConfig
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.TableQuery

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by andrei on 5/22/17.
  */


object AMail extends App {
  def printHelp() = {
    println("email indexer")
  }
  val configFile = new File(System.getProperty("user.home") + "/.config/amkhlv/amail.conf")
  if (!configFile.exists()) {
    throw new Exception("Configuration file " + configFile.getAbsolutePath + " not found")
  }
  val conf: Config = ConfigFactory.parseFile(configFile)
  val maildirs: List[String] = conf.getStringList("maildirs").toList
  val filesToProcess: List[File] = (for (mdir <- maildirs) yield (new File(mdir)).listFiles.filter(_.isFile)).flatten
  val knownHeaders: List[String] = List("message-id", "from", "to", "cc", "bcc", "subject", "date", "content-type")
  val nowMilliSeconds: Long = Instant.now().toEpochMilli()
  val handler: EMailContentHandler = new EMailContentHandler(knownHeaders.toArray)
  val mimeConfBuilder: MimeConfig.Builder = new MimeConfig.Builder()
  mimeConfBuilder.setMaxHeaderLen(100000)
  mimeConfBuilder.setMaxLineLen(10000)
  val mimeConf: MimeConfig = mimeConfBuilder.build()
  val parser: MimeStreamParser = new MimeStreamParser(mimeConf)
  parser.setContentHandler(handler)

  val sqliConfig = new SQLiteConfig();
  sqliConfig.setJournalMode(SQLiteConfig.JournalMode.MEMORY)
  sqliConfig.setSynchronous(SQLiteConfig.SynchronousMode.OFF)
  val emailsDB = Database.forURL(
    "jdbc:sqlite:" + conf.getString("dbfile"),
    driver = "org.sqlite.JDBC",
    prop = sqliConfig.toProperties
  )
  val emailsToInsert: TableQuery[Emails] = TableQuery[Emails]
  val batchSize : Int = 10000
  def getKnownMsgIds = sql"""select MSGID from EMAILS""".as[String]

  @tailrec def insertionSequence(knownMsgIds: Vector[String],
                                 fs: List[File],
                                 acc: List[DBIO[Int]]
                                ): Future[List[Int]] =
    (fs, acc.length) match {
      case (List(), _)      => emailsDB.run(DBIO.sequence(acc))
      case (fs , i) if (i >= batchSize) => {
        fs match {
          case (f :: fsrst) => println("\ninserting batch of " + batchSize + " until " + f)
          case List() => println("\n inserting last batch")
        }
        val insBatch = emailsDB.run(DBIO.sequence(acc))
        Await.ready(insBatch, 10 minute)
        insertionSequence(knownMsgIds, fs, List())
      }
      case (f :: rest, _)   => {
        val fh = new FileInputStream(f)
        try {
          parser.parse(fh)
        } catch {
          case e: Exception => {
            println()
            println(f.toPath)
            println(e.getMessage)
            handler.collectedHeaders.clear()
          }
        } finally {
          fh.close()
        }
        val hdrs = handler.collectedHeaders
        //        for (k <- handler.knownHeaders) yield {
        //          println(k + "--->" + hdrs.get(k));
        //        }
        Option(hdrs.get("message-id")) match {
          case Some(mid) => {
            if (knownMsgIds.contains(mid)) {
              print("-")
              hdrs.clear()
              insertionSequence(knownMsgIds, rest, acc)
            } else {
              print("+")
              val p = f.getAbsolutePath
              val eml = Email(
                mid,
                Option(hdrs.get("from")).map(decodeMime),
                Option(hdrs.get("to")).map(decodeMime),
                Option(hdrs.get("cc")).map(decodeMime),
                Option(hdrs.get("bcc")).map(decodeMime),
                Option(hdrs.get("subject")).map(decodeMime),
                Option(hdrs.get("date")).map(parseData),
                Option(hdrs.get("date")),
                Option(hdrs.get("content-type")).map(decodeMime),
                p.substring(0, p.lastIndexOf(':'))
              )
              hdrs.clear()
              insertionSequence(knownMsgIds.+:(mid), rest, (emailsToInsert += eml) :: acc)
            }
          }
          case None => {
            println("Error Message-Id not found in " + f.getAbsolutePath + " :")
            for (k <- hdrs.keySet()) { println(k + "-->" + hdrs.get(k) + "<--")}
            hdrs.clear()
            insertionSequence(knownMsgIds, rest, acc)
          }
        }
      }
    }
  def goIndex(lookback: Option[Long]) = {
    val fs = lookback match {
      case None    => filesToProcess
      case Some(m) => filesToProcess.filter(p => (nowMilliSeconds - p.lastModified()) < (m * 86400 * 1000))
    }
    val insFuture = emailsDB.run(getKnownMsgIds).flatMap(v => insertionSequence(v, fs, List()))
    Await.result(insFuture, 60 minutes)
  }

  args.toList match {
      case List() => goIndex(None)
      case List("--help") => printHelp()
      case "--lookback" :: n :: rest => goIndex(Some(n.toLong))
    }
}
