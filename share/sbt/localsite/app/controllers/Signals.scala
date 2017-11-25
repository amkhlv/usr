package controllers

case class UpdateEvent(icalFile: java.io.File, uid: String, efd: EventFormData)

case class NewEvent(icalFile: java.io.File,  efd: EventFormData)

case class DelEvent(icalFile: java.io.File, uid: String)

case class CheckMainWin()

case class ApprovalRequest(text: String, token: String)

case class ApprovalResponse(r: Boolean, token: String)

case class ShowWarning(text: String)

case class AskPass()
