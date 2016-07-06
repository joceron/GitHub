package util

import play.api.http.{MimeTypes, HeaderNames}
import play.api.libs.ws._
import play.api.mvc.{Results, Action, Controller}
import play.api.Configuration
import play.api.db._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import javax.inject._

import models._

class OAuth2(configuration: Configuration, ws: WSClient) {
  lazy val githubAuthId = configuration.getString("github.client.id").get
  lazy val githubAuthSecret = configuration.getString("github.client.secret").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
    val baseUrl = configuration.getString("github.redirect.url").get
    baseUrl.format(githubAuthId, redirectUri, scope, state)
  }

  def getToken(code: String): Future[String] = {
    val tokenResponse = ws.url("https://github.com/login/oauth/access_token").
      withQueryString("client_id" -> githubAuthId,
        "client_secret" -> githubAuthSecret,
        "code" -> code).
      withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).
      post(Results.EmptyContent())

    tokenResponse.flatMap { response =>
      (response.json \ "access_token").asOpt[String].fold(Future.failed[String](new IllegalStateException("Sod off!"))) { accessToken =>
        Future.successful(accessToken)
      }
    }
  }
}

object GitHubAPI {
  def getRepos(accesToken: String, ws: WSClient) = ws.url("https://api.github.com/user/repos").
    withHeaders(HeaderNames.AUTHORIZATION -> s"token $accesToken").
    get()

  def getIssues(repoName: String, ownerName: String, accesToken: String, ws: WSClient) = ws.url(s"https://api.github.com/repos/$ownerName/$repoName/issues").
    withHeaders(HeaderNames.AUTHORIZATION -> s"token $accesToken").
    get()
}

class AuthController @Inject()(val messagesApi: MessagesApi, configuration: Configuration, ws: WSClient, db: Database) extends Controller with I18nSupport {
  lazy val oauth2 = new OAuth2(configuration, ws)

  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
    (for {
      code <- codeOpt
      state <- stateOpt
      oauthState <- request.session.get("oauth-state")
    } yield {
      if (state == oauthState) {
        oauth2.getToken(code).map { accessToken =>
          Redirect(util.routes.AuthController.success()).withSession("oauth-token" -> accessToken)
        }.recover {
          case ex: IllegalStateException => Unauthorized(ex.getMessage)
        }
      } else {
        Future.successful(BadRequest("Invalid github login"))
      }
    }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
  }

  def success() = Action.async { request =>
    request.session.get("oauth-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
      val futureResponse = GitHubAPI.getRepos(authToken, ws)
      futureResponse.flatMap { response =>
        val repositories = response.json.as[Seq[Repository]]

        val futureModels = for (repository <- repositories) yield {
          GitHubAPI.getIssues(repository.repoName, repository.owner.ownerName, authToken, ws).map { response =>
            val issues = response.json.as[Seq[Issue]]
            MainModel(repository, issues)
          }
        }

        val conn = db.getConnection()
        try {
          val stmt = conn.createStatement
          val rs = stmt.executeQuery("SELECT ID, HOURS FROM TIMETABLE")

          val dataFromDB = new Iterator[IssueHour] {
            def hasNext = rs.next()

            def next() = new IssueHour(rs.getLong("ID"), rs.getInt("HOURS"))
          }.toList
          val form = new FormModel(dataFromDB)

          Future.sequence(futureModels).map(mainModel =>
            Ok(views.html.repository(mainModel, FormModel.createFormModel.fill(form)))
          )
        } finally {
          conn.close()
        }
      }
    }
  }

  def processForm() = Action.async { implicit request =>
    request.session.get("oauth-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
      val futureResponse = GitHubAPI.getRepos(authToken, ws)
      futureResponse.flatMap { response =>
        val repositories = response.json.as[Seq[Repository]]

        val futureModels = for (repository <- repositories) yield {
          GitHubAPI.getIssues(repository.repoName, repository.owner.ownerName, authToken, ws).map { response =>
            val issues = response.json.as[Seq[Issue]]
            MainModel(repository, issues)
          }
        }

        val userData = FormModel.createFormModel.bindFromRequest.get.issuesHours

        val conn = db.getConnection()
        try {
          val stmt = conn.createStatement
          for (issueHour <- userData)
            stmt.executeUpdate("UPDATE TIMETABLE SET HOURS=" + issueHour.time + " WHERE ID=" + issueHour.idIssue + ";")
          val rs = stmt.executeQuery("SELECT ID, HOURS FROM TIMETABLE")
          val dataFromDB = new Iterator[IssueHour] {
            def hasNext = rs.next()

            def next() = new IssueHour(rs.getLong("ID"), rs.getInt("HOURS"))
          }.toList
          val form = new FormModel(dataFromDB)

          Future.sequence(futureModels).map(mainModel =>
            Ok(views.html.repository(mainModel, FormModel.createFormModel.fill(form))))
        } finally {
          conn.close()
        }
      }
    }
  }
}

//Useful code, maybe later
//
//val allIssuesIdModel = mainModel.flatMap(repository =>
//  repository.issues.map(issue => issue.id)
//)
//val allIssuesIdDb = dataFromDB.map(issueHour => issueHour.idIssue)
//val nonExistingIssues = allIssuesIdModel.filter(!allIssuesIdDb.contains(_))
//val finalSeq = nonExistingIssues.map(issueId => new IssueHour(issueId,0)) ++ dataFromDB
//val form = new FormModel(finalSeq)
