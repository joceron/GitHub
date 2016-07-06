package models

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Owner(ownerName: String, id: Long)

object Owner {
  implicit val ownerReads: Reads[Owner] = (
    (JsPath \ "login").read[String] and
      (JsPath \ "id").read[Long]) (Owner.apply _)
}

case class Repository(repoName: String, owner: Owner)

object Repository {
  implicit val repositoryReads: Reads[Repository] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "owner").read[Owner]) (Repository.apply _)
}

case class Label(text: String, color: String)

object Label {
  implicit val labelReads: Reads[Label] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "color").read[String]) (Label.apply _)
}

case class Assignee(id: Long, pictureURL: String)

object Assignee {
  implicit val assigneeReads: Reads[Assignee] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "avatar_url").read[String]) (Assignee.apply _)
}

case class Milestone(id: Long, milestoneName: String)

object Milestone {
  implicit val milestoneReads: Reads[Milestone] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "title").read[String]) (Milestone.apply _)
}

case class Issue(id: Long, URL: String, issueName: String, labels: Seq[Label], assignees: Seq[Assignee],
                 milestone: Milestone, created: String, updated: String)

object Issue {
  val inputDateFormat = "YYYY-MM-DD'T'HH:mm:SSZ"
  val outputDateFormat = "DD-MM-YYYY"
  val jodaDateReads = Reads[String](js =>
    js.validate[String].map[String](dtString => {
      val inputDate = DateTime.parse(dtString, DateTimeFormat.forPattern(inputDateFormat))
      val formatter = DateTimeFormat.forPattern(outputDateFormat)
      formatter.print(inputDate)
    })
  )
  implicit val issueReads: Reads[Issue] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "html_url").read[String] and
      (JsPath \ "title").read[String] and
      (JsPath \ "labels").read[Seq[Label]] and
      (JsPath \ "assignees").read[Seq[Assignee]] and
      (JsPath \ "milestone").read[Milestone].orElse(Reads.pure(new Milestone(0, "(No milestone)"))) and
      (JsPath \ "created_at").read[String](jodaDateReads) and
      (JsPath \ "updated_at").read[String](jodaDateReads)) (Issue.apply _)
}

case class MainModel(repository: Repository, issues: Seq[Issue])
