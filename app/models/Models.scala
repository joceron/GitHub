package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Owner(ownerName: String, id: Long)

object Owner {
  implicit val ownerReads: Reads[Owner] = (
    (JsPath \ "login").read[String] and
    (JsPath \ "id").read[Long])(Owner.apply _)
}

case class Repository(repoName: String, owner: Owner)

object Repository {
  implicit val repositoryReads: Reads[Repository] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "owner").read[Owner])(Repository.apply _)
}

case class Milestone(id: Long, milestoneName: String)

object Milestone {
  implicit val milestoneReads: Reads[Milestone] = (
    (JsPath \ "id").read[Long] and
    (JsPath \ "title").read[String])(Milestone.apply _)
}

case class Issue(id: Long, issueName: String, milestone: Option[Milestone])

object Issue {
  implicit val issueReads: Reads[Issue] = (
    (JsPath \ "id").read[Long] and
    (JsPath \ "title").read[String] and
    (JsPath \ "milestone").readNullable[Milestone])(Issue.apply _)
}

case class MainModel(repository: Repository, issues: Seq[Issue])
