package controllers

import java.util.UUID

import org.sedis.Dress
import org.sedis.Dress.delegateToJedis

import com.typesafe.plugin.RedisPlugin
import com.typesafe.plugin.use

import model.Trida
import model.Zak
import model.mappings.UUIDMapping
import model.views.Selectable
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.optional
import play.api.mvc.Action
import play.api.mvc.Controller

object ZakController extends Controller {

  val zakMapping = mapping(
    "uuidZak" -> optional(UUIDMapping.uuidType),
    "jmeno" -> nonEmptyText,
    "prijmeni" -> nonEmptyText,
    "uuidTrida" -> UUIDMapping.uuidType)(Zak.apply)(Zak.unapply)

  val zakForm = Form(zakMapping)

  def create = Action { implicit request =>
    Ok(views.html.zak.create(zakForm, routes.ZakController.save, Application.getSelectableTridy()))
  }

  def save = Action { implicit request =>
    val form = zakForm.bindFromRequest
    form.fold(
      formWithErrors => {
        Ok(views.html.zak.create(formWithErrors, routes.ZakController.save, Application.getSelectableTridy()))
      },
      zak => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          val uuidZak = UUID.randomUUID.toString

          val sedis = Dress.up(client)

          sedis.sadd("zaci", uuidZak)

          sedis.hset(s"zak:$uuidZak", "jmeno", zak.jmeno)
          sedis.hset(s"zak:$uuidZak", "prijmeni", zak.prijmeni)
          sedis.hset(s"zak:$uuidZak", "uuidTridy", zak.uuidTrida.toString)

          sedis.sadd(zak.uuidTrida.toString, uuidZak) // pridame do tridy uuidTrida zaka s uuidZak

          Redirect(routes.Application.index())
        }
      })
  }

  def edit = Action {
    Ok("Zak edit")
  }
}