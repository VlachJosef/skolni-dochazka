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

object ApplicationRoutes extends Controller  {

  def jsRoutes = Action { implicit request =>
    val routes = Routes.javascriptRouter("appRoutes")(
      controllers.routes.javascript.TridaController.editTrida,
      controllers.routes.javascript.TridaController.update,
      controllers.routes.javascript.TridaController.delete,
      controllers.routes.javascript.ZakController.delete,
      controllers.routes.javascript.BackupController.delete,
      controllers.routes.javascript.BackupController.restore,
      controllers.routes.javascript.DochazkaController.put,
      controllers.routes.javascript.DochazkaController.update,
      controllers.routes.javascript.DochazkaController.delete
      )
    Ok(routes).as("text/javascript")
  }
}