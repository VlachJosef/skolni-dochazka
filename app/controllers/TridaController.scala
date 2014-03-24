package controllers

import java.util.UUID

import org.sedis.Dress
import org.sedis.Dress.delegateToJedis

import com.typesafe.plugin.RedisPlugin
import com.typesafe.plugin.use

import play.api.Play.current
import play.api.data.Form
import play.api.data.FormError
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.single
import play.api.mvc.Action
import play.api.mvc.Controller

object TridaController extends Controller {

  val classForm = Form(
    single("nazevTridy" -> nonEmptyText))

  def create = Action { implicit request =>
    Ok(views.html.trida.create(classForm, routes.TridaController.save))
  }

  def save = Action { implicit request =>
    val form = classForm.bindFromRequest
    form.fold(
      formWithErrors => {
        Ok(views.html.trida.create(formWithErrors, routes.TridaController.save))
      },
      nazevTridy => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          if (Dress.up(client).sadd("tridy", nazevTridy) == 0) {
            val errors = Seq(FormError("nazevTridy", "error.nazevTridy.already.exists", nazevTridy))
            val formWithError = form.copy(errors = errors)
            Ok(views.html.trida.create(formWithError, routes.TridaController.save))
          } else {
            Dress.up(client).hset(s"trida:$nazevTridy", "uuid", UUID.randomUUID.toString)
            Redirect(routes.Application.index())
          }
        }
      })
  }
}