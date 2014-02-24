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

  val formatter = DateTimeFormat.forPattern("dd.MM.yyyy")

  def prehledByUUIDTrida(uuidTrida: String) = Action { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val trida = Trida.getByUUID(uuidTrida, client)
      val zaci = Zak.getByUUIDTrida(uuidTrida, client);
      val dny = Dochazka.getDnyDochazkyByUUidTrida(uuidTrida, client).toList.sortBy(formatter.parseLocalDate(_))

      val chartsData = zaci.map(zak => {
        val denPocetHodin = for (
          den <- dny.toList
        ) yield {
          val pocetHodin = client.get(s"dochazka:$den:${zak.uuidZak.get.toString}")
          (den, pocetHodin)
        }
        val dnyList = denPocetHodin.map(_._1).map("'" + _ + "'")
        val dochazkaList = denPocetHodin.map(_._2.toInt).toList
        val kumulativniAbsenceData = for (i <- 1 to dochazkaList.size) yield {
          val x = dochazkaList.take(i)
          x.foldLeft(0)((acc, i) => acc + (4 - i))
        }
        val absenceAndHodiny = kumulativniAbsenceData.toList match {
          case head :: xs =>
            val zameskaneHodiny = kumulativniAbsenceData.reverse.head
        	val absence = (((zameskaneHodiny.toDouble*100.0)/(dochazkaList.size.toDouble * 4.0) * 100.0).toInt/100.0)
        	(absence, zameskaneHodiny)
          case _ => (0.0, 0)
        }
        Chart(zak, dnyList.mkString(","), dochazkaList.mkString(","), kumulativniAbsenceData.mkString(","), absenceAndHodiny._1, absenceAndHodiny._2)
      })
      Ok(views.html.dochazka.records(trida, chartsData))
    }
  }

  def evidujDochazkuByUUIDTrida(uuidTrida: String) = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val slozeniTridy = Skola.getSlozeniTridy(uuidTrida, client)
      Ok(views.html.dochazka.create(dochazkaForDayForm, slozeniTridy, Dochazka.getDochazkaTable(uuidTrida, client)))
    }
  }

  def save(uuidTrida: String) = DochazkaSecuredAction { implicit request =>
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

  def put = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
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

  def update = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
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

  def delete = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
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