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
import model.Sinner
import redis.clients.jedis.Jedis

object Application extends Controller {

  def jedisClient[T](body: Jedis => T): T = {
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { implicit client =>
      body(client)
    }
  }

  def withSedis[T](body: org.sedis.Dress.Wrap => T)(implicit client: Jedis): T = {
    body(Dress.up(client))
  }
  
  def index = Action {
    val skola = jedisClient { implicit client => Skola.getSkola }
    Ok(views.html.index(skola))
  }

  def getSelectableTridy() = {
    jedisClient { implicit client =>
      Selectable(Trida.getAll)((trida: Trida) => trida.uuidTrida.get.toString -> trida.nazev)
    }
  }
}

trait DochazkaSecured extends securesocial.core.SecureSocial {
  import controllers.Application.jedisClient
  def DochazkaSecuredAction(f: => Request[AnyContent] => SimpleResult) =
    //Action {implicit request => f(request)} // pro vypnutí security
    SecuredAction { implicit request =>
      val userName = request.user
      userName.email match {
        case Some("vlach.josef@gmail.com") | Some("katkahlavacova@centrum.cz") => f(request)
        case Some(email) => {
          jedisClient { implicit client =>
            Sinner.saveSinner(email)
            val sinners = Sinner.allSinners
            Results.Forbidden(views.html.sinners(sinners))
          }
        }
        case _ => Results.Forbidden
      }
    }
}
