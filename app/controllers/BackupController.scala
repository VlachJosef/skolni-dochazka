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
import java.io.FileOutputStream
import scala.collection.JavaConverters._
import java.io.File
import org.joda.time.LocalDateTime
import java.io.FileInputStream
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.i18n.Messages

object BackupController extends Controller with DochazkaSecured {

  val dumpsDirName = "dumps"

  def backup = DochazkaSecuredAction { implicit request =>
    getOrCreateDumpsDir() match {
      case Some(dir) => {
        val dumpFiles = dir.listFiles();
        val backupNames = dumpFiles.map(key => key.getName)
        Ok(views.html.backup.backup(backupNames))
      }
      case None => Ok("error pri nacitani backupu")
    }
  }

  def dump() = DochazkaSecuredAction { implicit request =>
    val pool = use[RedisPlugin].sedisPool
    pool.withJedisClient { client =>
      val keys = client.keys("*")
      getOrCreateDumpsDir().map(dir => {
        val localDateDir = new File(dir.getPath + "/" + LocalDateTime.now.toString)
        localDateDir.mkdir
        keys.asScala.map(key => {
          val dump = client.dump(key)
          val fos: FileOutputStream = new FileOutputStream(localDateDir.getPath + "/" + key)
          fos.write(dump, 0, dump.length);
          fos.flush();
          fos.close();
        })
        Redirect(routes.BackupController.backup)
      }).getOrElse(Ok("failed trying to create the directory for dump"))
    }
  }

  private def getOrCreateDumpsDir(): Option[File] = {
    val dir = new File(dumpsDirName);
    if ((dir.exists && dir.isDirectory) || dir.mkdir) {
      Some(dir)
    } else {
      None
    }
  }

  private def getIfExistsBackupDir(backupDir: String): Option[File] = {
    val dir = new File(dumpsDirName + "/" + backupDir)
    if ((dir.exists && dir.isDirectory)) {
      Some(dir)
    } else {
      None
    }
  }

  def restore = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[String](backupReads).map { backup =>
      val pool = use[RedisPlugin].sedisPool
      pool.withJedisClient { client =>
        getIfExistsBackupDir(backup).map(dir => {
          client.flushDB
          val dumpFiles = dir.listFiles();
          dumpFiles.foreach(key => {
            val ss = scala.io.Source.fromFile(key, "ISO8859-1")
            val keyContents = ss.map(_.toByte).toArray
            client.restore(key.getName, 0, keyContents)
            ss.close
          })
          Ok(Json.obj("message" -> Messages("success.restore.backup", backup)))
        }).getOrElse(BadRequest(Json.obj("message" -> "Chyba při obnovování zálohy")))
      }
    }.recoverTotal { e =>
      BadRequest(Json.obj("message" -> e.toString))
    }
  }

  implicit val backupReads: Reads[String] =
    (JsPath \ "backup").read[String]

  def delete = SecuredAction(ajaxCall = true)(parse.json) { implicit request =>
    request.body.validate[String](backupReads).map { backup =>
      getIfExistsBackupDir(backup) match {
        case Some(dir) => {
          val dumpFiles = dir.listFiles();
          dumpFiles.foreach(key => {
            key.delete
          })
          dir.delete
          Ok(Json.obj("message" -> Messages("success.delete.backup", backup)))
        }
        case None =>
          BadRequest(Json.obj("message" -> "Chyba při mazání zálohy"))
      }
    }.recoverTotal { e =>
      BadRequest(Json.obj("message" -> e.toString))
    }
  }
}