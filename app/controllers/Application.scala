package controllers

import java.util.UUID

import play.api.mvc._
import play.api.Configuration
import play.api.libs.ws._
import util.OAuth2

import javax.inject._

class Application @Inject()(configuration: Configuration, ws: WSClient) extends Controller {

  def index = Action { implicit request =>
    val oauth2 = new OAuth2(configuration, ws)
    val callbackUrl = util.routes.AuthController.callback(None, None).absoluteURL()
    val scope = "repo" // github scope - request repo access
  val state = UUID.randomUUID().toString // random confirmation string
  val redirectUrl = oauth2.getAuthorizationUrl(callbackUrl, scope, state)
    Ok(views.html.index("Your new application is ready.", redirectUrl)).
      withSession("oauth-state" -> state)
  }

}