package amail

import slick.jdbc.SQLiteProfile.api._
/**
  * Created by andrei on 5/22/17.
  */
class CaseClassMapping {

}
case class User(name: String, lastname: Option[String])
class Users(tag: Tag) extends Table[User](tag, "USERS") {
  def name = column[String]("NAME")

  def lastname = column[String]("LAST")

  // the * projection (e.g. select * ...) auto-transforms the tupled
  // column values to / from a User
  def * = (name, lastname.?) <> (User.tupled, User.unapply _)
}

case class Email(
                  messageId: String,
                  fromWhom: Option[String],
                  toWhom: Option[String],
                  cc: Option[String],
                  bcc: Option[String],
                  subject: Option[String],
                  data: Option[String],
                  fulldata: Option[String],
                  contains: Option[String],
                  filepath: String
                )
class Emails(tag: Tag) extends Table[Email](tag, "EMAILS") {
  def messageId = column[String]("msgid")
  def fromWhom  = column[String]("f")
  def toWhom    = column[String]("t")
  def cc        = column[String]("cc")
  def bcc       = column[String]("bcc")
  def subject   = column[String]("s")
  def data      = column[String]("d")
  def fulldata  = column[String]("data")
  def contains  = column[String]("c")
  def filepath  = column[String]("p")
  def * = (messageId, fromWhom.?, toWhom.?, cc.?, bcc.?, subject.?, data.?, fulldata.?, contains.?, filepath) <> (Email.tupled, Email.unapply _)
}