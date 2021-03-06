package controllers

import controllers.Application.jedisClient
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
    val skola = jedisClient { implicit client =>
      Skola.getSkola
    }
    Ok(views.html.summary(skola))
  }

  def Asc[T: Ordering] = implicitly[Ordering[T]]
  def Desc[T: Ordering] = implicitly[Ordering[T]].reverse

  def prehledByUUIDTridaOrdered(uuidTrida: String, orderBy: String, direction: String) = Action { implicit request =>
    jedisClient { implicit client =>
      val trida = Trida.getByUUID(uuidTrida)
      val chartsData = Chart.get(uuidTrida)

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
    jedisClient { implicit client =>
      val trida = Trida.getByUUID(uuidTrida)
      val chartsData = Chart.get(uuidTrida)

      Ok(views.html.dochazka.records(trida, chartsData))
    }
  }

  def editDochazkaByUUIDTrida(uuidTrida: String, typVyuky: String) = DochazkaSecuredAction { implicit request =>
    jedisClient { implicit client =>
      val slozeniTridy = Skola.getSlozeniTridy(uuidTrida)
      val slozeniTridyFilter = slozeniTridy.copy(zaci = slozeniTridy.zaci.filter(Dochazka.aktivityFilter(typVyuky)))
      val dochazkaTable = Dochazka.getDochazkaTable(uuidTrida, typVyuky)
      val dochazkaTableFilter = dochazkaTable.copy(header = dochazkaTable.header.copy(zaci = dochazkaTable.header.zaci.filter(Dochazka.aktivityFilter(typVyuky))))
      Ok(views.html.dochazka.create(dochazkaForDayForm, slozeniTridyFilter, dochazkaTableFilter, typVyuky))
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
      jedisClient { implicit client =>
        Dochazka.saveDochazka(dochazka, typVyuky)
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
      jedisClient { implicit client =>
        Dochazka.updateDochazka(dochazkaUpdate, typVyuky)
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
      jedisClient { implicit client =>
        Dochazka.deleteDay(tuple, typVyuky)
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
    jedisClient { implicit client =>
      val sinners = Sinner.allSinners
      Ok(views.html.dochazka.sinners(sinners))
    }
  }
}