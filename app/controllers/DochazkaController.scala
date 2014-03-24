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

object DochazkaController extends Controller {

  val dochazkaForDayForm = Form(
    mapping(
      "den" -> jodaLocalDate(pattern = "dd.MM.yyyy"),
      "uuidTrida" -> nonEmptyText,
      "dochazka" -> play.api.data.Forms.list {
        mapping("uuidZak" -> nonEmptyText,
          "pocetHodin" -> play.api.data.Forms.of[Int])(Pritomnost.apply)(Pritomnost.unapply)
      })(Dochazka.apply)(Dochazka.unapply))

  def prehledByUUIDTrida(uuidTrida: String) = Action {
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val trida = Trida.getByUUID(uuidTrida, client)
      val zaci = Zak.getByUUIDTrida(uuidTrida, client);
      val dny = Dochazka.getDnyDochazkyByUUidTrida(uuidTrida, client)

      val chartsData = zaci.map(zak => {
        val denPocetHodin = for (
          den <- dny.toList
        ) yield {
          val pocetHodin = client.get(s"dochazka:$den:${zak.uuidZak.get.toString}")
          (den, pocetHodin)
        }
        val dnyList = denPocetHodin.map(_._1).toList.sortBy(_.toString).map("'" + _ + "'")
        val dochazkaList = denPocetHodin.map(_._2).toList
        val dochazkaListDouble = dochazkaList.map(pocetHodin => pocetHodin.toDouble)
        val kumulativniAbsenceData = for (i <- 1 to dochazkaListDouble.size) yield {
          val x = dochazkaListDouble.take(i)
          x.foldLeft(0.0)(_ + _)
        }
        Chart(zak, dnyList.mkString(","), dochazkaList.mkString(","), kumulativniAbsenceData.mkString(","))
      })
      Ok(views.html.dochazka.records(trida, chartsData))
    }
  }

  def evidujDochazkuByUUIDTrida(uuidTrida: String) = Action {
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val slozeniTridy = Skola.getSlozeniTridy(uuidTrida, client)
      Ok(views.html.dochazka.create(dochazkaForDayForm, slozeniTridy, Dochazka.getDochazkaTable(uuidTrida, client)))
    }
  }

  def save(uuidTrida: String) = Action { implicit request =>
    val form = dochazkaForDayForm.bindFromRequest
    form.fold(
      formWithErrors => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          val slozeniTridy = Skola.getSlozeniTridy(uuidTrida, client)
          Ok(views.html.dochazka.create(formWithErrors, Skola.getSlozeniTridy(uuidTrida, client), Dochazka.getDochazkaTable(uuidTrida, client)))
        }
      },
      dochazka => {
        val pool = use[RedisPlugin].sedisPool
        pool.withJedisClient { client =>
          Dochazka.saveDochazka(dochazka, client)
          Redirect(routes.DochazkaController.prehledByUUIDTrida(uuidTrida))
        }
      })
  }

  val localDateRead = jodaLocalDateReads("dd.MM.yyyy")
  implicit val dochazkaReads: Reads[Pritomnost] = (
    (__ \ "uuidZak").read[String] and
    (__ \ "pocetHodin").read[Int])(Pritomnost.apply _)

  implicit val tridaReads: Reads[Dochazka] = (
    (__ \ "den").read[LocalDate](localDateRead) and
    (__ \ "uuidTrida").read[String] and
    (__ \ "dochazka").read[List[Pritomnost]])(Dochazka.apply _)

  def put = Action(parse.json) { implicit request =>
    request.body.validate[Dochazka](tridaReads).map { dochazka =>
      val pool = use[RedisPlugin].sedisPool
      pool.withJedisClient { client =>
        Dochazka.saveDochazka(dochazka, client)
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

  def update = Action(parse.json) { implicit request =>
    request.body.validate[DochazkaUpdate](updateReads).map { dochazkaUpdate =>
      val pool = use[RedisPlugin].sedisPool
      pool.withJedisClient { client =>
        Dochazka.updateDochazka(dochazkaUpdate, client)
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

  def delete = Action(parse.json) { implicit request =>
    request.body.validate[(LocalDate, String)](deleteDayReads).map { tuple =>
      val pool = use[RedisPlugin].sedisPool
      pool.withJedisClient { client =>
        Dochazka.deleteDay(tuple, client)
        Ok(Json.obj("message" -> Messages("success.delete.dochazka.day")))
      }
    }.recoverTotal { e =>
      {
        // TODO logovani chyby pri
        BadRequest(Json.obj("message" -> Messages("error.delete.dochazka.day")))
      }
    }
  }

}