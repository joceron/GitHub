package models

import play.api.data._
import play.api.data.Forms._

//TODO: time will become a Date
case class IssueHour(idIssue: Long, time: Int)

case class FormModel(issuesHours: Seq[IssueHour])

object FormModel {
  val createFormModel = Form(
    mapping(
      "issuesHours" -> seq(
        mapping(
          "idIssue" -> longNumber,
          "time" -> number)(IssueHour.apply)(IssueHour.unapply)
      )
    )(FormModel.apply)(FormModel.unapply)
  )
}
