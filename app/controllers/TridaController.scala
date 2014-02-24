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
import play.api.data.Forms.mapping
import play.api.data.Forms.optional
import play.api.mvc.Action
import play.api.mvc.Controller
import model.Trida
import model.views.Selectable
import play.api.Routes
import model.mappings.UUIDMapping
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.i18n.Messages
import model.Zak

object TridaController extends Controller with DochazkaSecured {

  implicit object UUIDFormat extends Format[UUID] {
    def writes(uuid: UUID): JsValue = JsString(uuid.toString())
    def reads(json: JsValue): JsResult[UUID] = json match {
      case JsString(x) => JsSuccess(UUID.fromString(x))
      case _ => JsError("Expected UUID as JsString")
    }
  }

  implicit val tridaReads: Reads[Trida] = (
    (JsPath \ "uuidTrida").read[Option[UUID]] and
    (JsPath \ "nazevTrida").read[String](minLength[String](1)))(Trida.apply _)

  val tridaMapping = mapping(
    "uuidTrida" -> optional(UUIDMapping.uuidType),
    "nazevTrida" -> nonEmptyText)(Trida.apply)(Trida.unapply)

  val tridaForm = Form(tridaMapping)

  def create = DochazkaSecuredAction { implicit request =>
    Ok(views.html.trida.create(tridaForm, routes.TridaController.save))
  }

  def save = DochazkaSecuredAction { implicit request =>
    val form = tridaForm.bindFromRequest
    form.fold(
      formWithErrors => {
        Ok(views.html.trida.create(formWithErrors, routes.TridaController.save))
      },
      trida => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          val nazevTridy = trida.nazev
          if (Dress.up(client).sadd("tridy", nazevTridy) == 0) {
            val errors = Seq(FormError("nazevTridy", "error.nazevTridy.already.exists", nazevTridy))
            val formWithError = form.copy(errors = errors)
            Ok(views.html.trida.create(formWithError, routes.TridaController.save))
          } else {
            val uuidTrida = UUID.randomUUID.toString
            Dress.up(client).hset(s"trida:$nazevTridy", "uuid", uuidTrida)
            Dress.up(client).hset(s"trida:$uuidTrida", "nazev", nazevTridy)
            Redirect(routes.DochazkaController.summary())
          }
        }
      })
  }

  def edit = DochazkaSecuredAction { implicit request =>
    Ok(views.html.trida.edit(tridaForm, Application.getSelectableTridy()))
  }

  def editTrida(uuidTrida: String) = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val trida = Trida.getByUUID(uuidTrida, client)
      val zaci = Zak.getByUUIDTrida(uuidTrida, client)
      Ok(views.html.trida.update(tridaForm.fill(trida), zaci))
    }
  }

  def update = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[Trida].map {
      case Trida(Some(uuidTrida), nazev) => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          val nazevOld = Dress.up(client).hget(s"trida:$uuidTrida", "nazev")
          if (nazevOld != null) {
            if (Dress.up(client).sadd("tridy", nazev) == 0) {
              BadRequest(Json.obj("message" -> Messages("error.already.exists", Messages("nazevTrida"), nazev)))
            } else {
              Dress.up(client).srem("tridy", nazevOld)
              client.del(s"trida:$nazevOld")
              Dress.up(client).hset(s"trida:$nazev", "uuid", uuidTrida.toString)
              Dress.up(client).hset(s"trida:$uuidTrida", "nazev", nazev)
              Ok(Json.obj(
                "nazevTrida" -> nazev,
                "uuidTrida" -> uuidTrida,
                "message" -> Messages("success.zmena.nazev.tridy", nazevOld, nazev)))
            }
          } else {
            BadRequest(Json.obj("message" -> Messages("error.trida.nenalezena", uuidTrida)))
          }
        }
      }
    }.recoverTotal { e =>
      {
        val errors = e.errors.map(fieldError => {
          val path = fieldError._1
          fieldError._2.map(valError => {
            Messages(valError.message, Messages(path.toString.substring(1)), valError.args(0))
          })
        }).flatten
        BadRequest(Json.obj("message" -> errors))
      }
    }
  }
  def delete = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[Trida].map {
      case Trida(Some(uuidTrida), nazev) => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          val nazevOld = Dress.up(client).hget(s"trida:$uuidTrida", "nazev")
          if (nazevOld != null) {
            Trida.deleteByUUID(uuidTrida.toString, client)
            Ok(Json.obj(
              "nazevTrida" -> nazev,
              "uuidTrida" -> uuidTrida,
              "message" -> Messages("success.delete.tridy", nazev)))
          } else {
            BadRequest(Json.obj("message" -> Messages("error.trida.nenalezena", uuidTrida)))
          }
        }
      }
    }.recoverTotal { e =>
      {
        val errors = e.errors.map(fieldError => {
          val path = fieldError._1
          fieldError._2.map(valError => {
            Messages(valError.message, Messages(path.toString.substring(1)), valError.args(0))
          })
        }).flatten
        BadRequest(Json.obj("message" -> errors))
      }
    }
  }
}