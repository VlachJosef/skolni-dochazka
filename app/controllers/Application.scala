package controllers

import org.sedis.Dress
import com.typesafe.plugin.RedisPlugin
import com.typesafe.plugin.use
import model.Skola
import model.SlozeniTridy
import model.Trida
import model.Zak
import play.api.Play.current
import play.api.Routes
import play.api.mvc.Action
import play.api.mvc.Controller
import model.views.Selectable
import play.api.mvc.Request
import play.api.mvc.AnyContent
import play.api.mvc.SimpleResult
import play.api.mvc.Results

object Application extends Controller {

  def index = Action {
    val pool = use[RedisPlugin].sedisPool
    val skola = pool.withJedisClient { client =>
      Skola.getSkola(client)
    }
    Ok(views.html.index(skola))
  }

  def getSelectableTridy() = {
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      Selectable(Trida.getAll(client))((trida: Trida) => trida.uuidTrida.get.toString -> trida.nazev)
    }
  }
}

trait DochazkaSecured extends securesocial.core.SecureSocial {
  def DochazkaSecuredAction(f: => Request[AnyContent] => SimpleResult) =
//    Action {implicit request => f(request)} // pro vypnutí security
    SecuredAction { implicit request =>
      val userName = request.user
      userName.email match {
        case Some("vlach.josef@gmail.com") => f(request)
        case Some("katkahlavacova@centrum.cz") => f(request)
        case Some(email) => Results.Forbidden("Na to ani nemysli milánku. " + email)
        case _ => Results.Forbidden
      }
    }
}
