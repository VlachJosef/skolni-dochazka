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
import play.api.data.FormError

object ZakController extends Controller with DochazkaSecured {

  val zakMapping = mapping(
    "uuidZak" -> optional(UUIDMapping.uuidType),
    "jmeno" -> nonEmptyText,
    "prijmeni" -> nonEmptyText,
    "poradoveCislo" -> number,
    "aktivity" -> boolean,
    "uuidTrida" -> UUIDMapping.uuidType)(Zak.apply)(Zak.unapply)

  val zakForm = Form(zakMapping)

  def add(uuidTrida: String) = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val trida = Trida.getByUUID(uuidTrida, client)
      val maxPoradoveCislo = Trida.getMaxPoradoveCisloZak(uuidTrida, client)
      val zak = Zak(None, "", "", maxPoradoveCislo, true, UUID.fromString(uuidTrida))
      Ok(views.html.zak.update(zakForm.fill(zak), routes.ZakController.update, Application.getSelectableTridy(), "legend.zak.novy"))
    }
  }

  def update = DochazkaSecuredAction { implicit request =>
    val form = zakForm.bindFromRequest
    form.fold(
      formWithErrors => {
        Ok(views.html.zak.update(formWithErrors, routes.ZakController.update, Application.getSelectableTridy(), "legend.zak.edit"))
      },
      zak => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          zak.uuidZak match {
            case Some(_) => Zak.update(zak, client)
            case None => Zak.save(zak, client)
          }
        }
        Redirect(routes.DochazkaController.summary())
      })
  }

  def editZak(uuidZak: String) = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val zak = Zak.getByUUIDZak(uuidZak, client)
      Ok(views.html.zak.update(zakForm.fill(zak), routes.ZakController.update, Application.getSelectableTridy(), "legend.zak.edit"))
    }
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