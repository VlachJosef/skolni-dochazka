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
import model.Skola
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.i18n.Messages

object ZakController extends Controller with DochazkaSecured {

  val zakMapping = mapping(
    "uuidZak" -> optional(UUIDMapping.uuidType),
    "jmeno" -> nonEmptyText,
    "prijmeni" -> nonEmptyText,
    "uuidTrida" -> UUIDMapping.uuidType)(Zak.apply)(Zak.unapply)

  val zakForm = Form(zakMapping)

  def create = DochazkaSecuredAction { implicit request =>
    Ok(views.html.zak.create(zakForm, routes.ZakController.save, Application.getSelectableTridy()))
  }

  def save = DochazkaSecuredAction { implicit request =>
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

  def edit = DochazkaSecuredAction {  implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val skola = Skola.getSkola(client)
      Ok(views.html.zak.edit(skola))
    }
  }

  def editZak(uuidZak: String) = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val zak = Zak.getByUUIDZak(uuidZak, client)
      Ok(views.html.zak.update(zakForm.fill(zak), routes.ZakController.update, Application.getSelectableTridy()))
    }
  }

  def update = DochazkaSecuredAction { implicit request =>
    val form = zakForm.bindFromRequest
    form.fold(
      formWithErrors => {
        Ok(views.html.zak.update(formWithErrors, routes.ZakController.update, Application.getSelectableTridy()))
      },
      zak => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          val uuidZak = zak.uuidZak.get.toString

          val sedis = Dress.up(client)

          if (sedis.sismember("zaci", zak.uuidZak.get.toString)) {
            sedis.hset(s"zak:$uuidZak", "jmeno", zak.jmeno)
            sedis.hset(s"zak:$uuidZak", "prijmeni", zak.prijmeni)

            println("$uuidZak " + uuidZak)

            val oldUUIDTridy = sedis.hget(s"zak:$uuidZak", "uuidTridy")
            val uuidTrida = zak.uuidTrida.toString
            if (oldUUIDTridy != uuidTrida) {
              sedis.hset(s"zak:$uuidZak", "uuidTridy", uuidTrida)
              sedis.srem(oldUUIDTridy, uuidZak) // presuneme zaka z puvodni tridy...
              sedis.sadd(uuidTrida, uuidZak) // ... do nove tridy
            }
          }
          Redirect(routes.ZakController.edit())
        }
      })
  }

  implicit object UUIDFormat extends Format[UUID] {
    def writes(uuid: UUID): JsValue = JsString(uuid.toString())
    def reads(json: JsValue): JsResult[UUID] = json match {
      case JsString(x) => JsSuccess(UUID.fromString(x))
      case _ => JsError("Expected UUID as JsString")
    }
  }

  implicit val zakReads: Reads[String] =
    (JsPath \ "uuidZak").read[String]

  def delete = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[String](zakReads).map { uuidZak =>
      {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          if (Zak.exists(uuidZak, client)) {
            val zak = Zak.deleteByUUIDZak(uuidZak, client)
            Ok(Json.obj(
              "uuidZak" -> uuidZak,
              "message" -> Messages("success.delete.zak", zak.jmeno + " " + zak.prijmeni)))
          } else {
            BadRequest(Json.obj("message" -> Messages("error.zak.nenalezen", uuidZak)))
          }
        }
      }
    }.recoverTotal { e =>
      {
        val errors = e.errors.map(fieldError => {
          val path = fieldError._1
          fieldError._2.map(valError => {
            Messages(valError.message, path.toString, valError.args(0))
          })
        }).flatten
        BadRequest(Json.obj("message" -> errors))
      }
    }
  }
}