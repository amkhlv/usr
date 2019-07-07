/**
  * Created by andrei on 5/25/17.
  */
import javax.mail.internet.MimeUtility
import java.io.UnsupportedEncodingException
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

package object amail {
  val myformatter = DateTimeFormatter.ofPattern("yyyyMM")
  val formatter = DateTimeFormatter.RFC_1123_DATE_TIME

  def parseData(rawDateString: String) = {
    val rds = """\s+$""".r.replaceAllIn(
      """\s+\(.*\)$""".r.replaceAllIn(
        """\s+""".r.replaceAllIn(
          rawDateString,
          " "),
        ""),
      "")
    try {
      val localDateTime = LocalDateTime.parse(rds, formatter);
      myformatter.format(localDateTime)
    } catch {
      case e: DateTimeParseException =>
        try { //only "GMT" is allowed in RFC_1123
          val localDateTime = LocalDateTime.parse(
            """\s$""".r.replaceFirstIn(rds.substring(0, rds.length() - 4), "") + " GMT",
            formatter
          )
          myformatter.format(localDateTime)
        } catch {
          case e2: DateTimeParseException => {
            println("\nerror parsing time -->" + rawDateString + "<--")
            rds
          }
        }
    }
  }

  def decodeMime(raw: String) =
    try {
      MimeUtility.decodeText(raw)
    } catch {
      case uee: UnsupportedEncodingException => raw
    }
}
