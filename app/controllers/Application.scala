package controllers

import org.sedis.Dress

import com.typesafe.plugin.RedisPlugin
import com.typesafe.plugin.use

import model.Skola
import model.SlozeniTridy
import model.Trida
import model.Zak
import play.api.Play.current
import play.api.mvc.Action
import play.api.mvc.Controller

object Application extends Controller {

  def index = Action {
    val pool = use[RedisPlugin].sedisPool
    val skola = pool.withJedisClient { client =>
      val tridy = Trida.getAll(client)
      val sedis = Dress.up(client)

      val SlozeniTrid = tridy.map(trida => {
        val zaciUuid = sedis.smembers(trida.uuidTrida.toString)
        val zaci = zaciUuid.map(uuidZak => {
          Zak.getByUUIDZak(uuidZak, client)
        })
        SlozeniTridy(trida, zaci)
      })
      Skola(SlozeniTrid)
    }
    Ok(views.html.index(skola))
  }
}
