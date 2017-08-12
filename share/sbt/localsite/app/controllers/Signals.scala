package controllers

case class UpdateEvent(icalFile: java.io.File, uid: String, efd: EventFormData)

case class NewEvent(icalFile: java.io.File,  efd: EventFormData)

case class DelEvent(icalFile: java.io.File, uid: String)
