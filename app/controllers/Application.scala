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
