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
import play.api.data.Form
import play.api.data.Forms._
import java.util.Date
import model.Dochazka
import model.Pritomnost
import org.joda.time.format.DateTimeFormat
import play.api.data.format.Formats._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import model.Chart
import org.joda.time.LocalDate
import model.Pritomnost
import play.api.i18n.Messages
import model.DochazkaUpdate
import com.github.nscala_time.time.Implicits._
import model.Sinner

object DochazkaController extends Controller with DochazkaSecured {

  val dochazkaForDayForm = Form(
    mapping(
      "den" -> jodaLocalDate(pattern = "dd.MM.yyyy"),
      "uuidTrida" -> nonEmptyText,
      "dochazka" -> play.api.data.Forms.list {
        mapping("uuidZak" -> nonEmptyText,
          "pocetHodin" -> play.api.data.Forms.of[Int])(Pritomnost.apply)(Pritomnost.unapply)
      })(Dochazka.apply)(Dochazka.unapply))

  def summary = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    val skola = pool.withJedisClient { client =>
      Skola.getSkola(client)
    }
    Ok(views.html.summary(skola))
  }

  def Asc[T: Ordering] = implicitly[Ordering[T]]
  def Desc[T: Ordering] = implicitly[Ordering[T]].reverse

  def prehledByUUIDTridaOrdered(uuidTrida: String, orderBy: String, direction: String) = Action { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val trida = Trida.getByUUID(uuidTrida, client)
      val chartsData = Chart.get(uuidTrida, client)

      val sorted = chartsData.sortBy(ch => {
        orderBy match {
          case "poradi" => ch.zak.poradoveCislo
          case _ => ch.summaryAbsence.absenceTotal
        }
      })(direction match {
        case "asc" => Asc
        case _ => Desc
      })
      Ok(views.html.dochazka.content(trida, sorted))
    }
  }

  val formatter = DateTimeFormat.forPattern("dd.MM.yyyy")

  def prehledByUUIDTrida(uuidTrida: String) = Action { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val trida = Trida.getByUUID(uuidTrida, client)
      val chartsData = Chart.get(uuidTrida, client)

      Ok(views.html.dochazka.records(trida, chartsData))
    }
  }

  def editDochazkaByUUIDTrida(uuidTrida: String, typVyuky: String) = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val slozeniTridy = Skola.getSlozeniTridy(uuidTrida, client)
      Ok(views.html.dochazka.create(dochazkaForDayForm, slozeniTridy, Dochazka.getDochazkaTable(uuidTrida, typVyuky, client), typVyuky))
    }
  }

  val localDateRead = jodaLocalDateReads("dd.MM.yyyy")
  implicit val dochazkaReads: Reads[Pritomnost] = (
    (__ \ "uuidZak").read[String] and
    (__ \ "pocetHodin").read[Int])(Pritomnost.apply _)

  implicit val tridaReads: Reads[Dochazka] = (
    (__ \ "den").read[LocalDate](localDateRead) and
    (__ \ "uuidTrida").read[String] and
    (__ \ "dochazka").read[List[Pritomnost]])(Dochazka.apply _)

  def put(typVyuky: String) = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[Dochazka](tridaReads).map { dochazka =>
      val pool = use[RedisPlugin].sedisPool
      pool.withJedisClient { client =>
        Dochazka.saveDochazka(dochazka, typVyuky, client)
        Ok(Json.obj("message" -> Messages("success.delete.backup")))
      }
    }.recoverTotal { e =>
      {
        val errors = e.errors.map(fieldError => {
          val path: JsPath = fieldError._1
          fieldError._2.map(valError => {
            val msg = valError.message match {
              case "error.expected.jsnumber" => "error.pocetHodin.required"
              case "error.expected.jodadate.format" => "error.denDochazky.required"
              case _ => valError.message
            }
            Json.obj("message" -> Messages(msg), "path" -> path.toString)
          })
        }).flatten
        BadRequest(Json.obj("message" -> errors))
      }
    }
  }

  implicit val updateReads: Reads[DochazkaUpdate] = (
    (__ \ "den").read[LocalDate](localDateRead) and
    (__ \ "uuidZak").read[String] and
    (__ \ "pocetHodin").read[Int])(DochazkaUpdate.apply _)

  def update(typVyuky: String) = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[DochazkaUpdate](updateReads).map { dochazkaUpdate =>
      val pool = use[RedisPlugin].sedisPool
      pool.withJedisClient { client =>
        Dochazka.updateDochazka(dochazkaUpdate, typVyuky, client)
        Ok(Json.obj("message" -> Messages("success.update.dochazka.day")))
      }
    }.recoverTotal { e =>
      {
        // TODO logovani chyby pri
        BadRequest(Json.obj("message" -> Messages("error.update.dochazka.day")))
      }
    }
  }

  implicit val deleteDayReads: Reads[(LocalDate, String)] = (
    (__ \ "den").read[LocalDate](localDateRead) and
      (__ \ "uuidTrida").read[String] tupled)

  def delete(typVyuky: String) = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[(LocalDate, String)](deleteDayReads).map { tuple =>
      val pool = use[RedisPlugin].sedisPool
      pool.withJedisClient { client =>
        Dochazka.deleteDay(tuple, typVyuky, client)
        Ok(Json.obj("message" -> Messages("success.delete.dochazka.day")))
      }
    }.recoverTotal { e =>
      {
        // TODO logovani chyby pri
        BadRequest(Json.obj("message" -> Messages("error.delete.dochazka.day")))
      }
    }
  }

  def sinners = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val sinners = Sinner.allSinners(client)
      Ok(views.html.dochazka.sinners(sinners))
    }
  }
}