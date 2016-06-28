package util

import play.api.Application
import play.api.Play
import play.api.http.{ MimeTypes, HeaderNames }
import play.api.libs.ws.WS
import play.api.mvc.{ Results, Action, Controller }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import models._

class OAuth2(application: Application) {
  lazy val githubAuthId = application.configuration.getString("github.client.id").get
  lazy val githubAuthSecret = application.configuration.getString("github.client.secret").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
    val baseUrl = application.configuration.getString("github.redirect.url").get
    baseUrl.format(githubAuthId, redirectUri, scope, state)
  }

  def getToken(code: String): Future[String] = {
    val tokenResponse = WS.url("https://github.com/login/oauth/access_token")(application).
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
  def getRepos(accesToken: String)(implicit app: Application) = WS.url("https://api.github.com/user/repos").
    withHeaders(HeaderNames.AUTHORIZATION -> s"token $accesToken").
    get()

  def getIssues(repoName: String, ownerName: String, accesToken: String)(implicit app: Application) = WS.url(s"https://api.github.com/repos/$ownerName/$repoName/issues").
    withHeaders(HeaderNames.AUTHORIZATION -> s"token $accesToken").
    get()
}

object OAuth2 extends Controller {
  lazy val oauth2 = new OAuth2(Play.current)

  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
    (for {
      code <- codeOpt
      state <- stateOpt
      oauthState <- request.session.get("oauth-state")
    } yield {
      if (state == oauthState) {
        oauth2.getToken(code).map { accessToken =>
          Redirect(util.routes.OAuth2.success()).withSession("oauth-token" -> accessToken)
        }.recover {
          case ex: IllegalStateException => Unauthorized(ex.getMessage)
        }
      } else {
        Future.successful(BadRequest("Invalid github login"))
      }
    }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
  }

  def success() = Action.async { request =>
    implicit val app = Play.current
    request.session.get("oauth-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
      val futureResponse = GitHubAPI.getRepos(authToken)
      futureResponse.flatMap { response =>
        val repositories = response.json.as[Seq[Repository]]

        val futureModels = for (repository <- repositories) yield {
          GitHubAPI.getIssues(repository.repoName, repository.owner.ownerName, authToken).map { response =>
            val issues = response.json.as[Seq[Issue]]
            MainModel(repository, issues)
          }
        }

        Future.sequence(futureModels).map(mainModel =>
          Ok(views.html.repository(mainModel)))
      }
    }
  }
}
