package model.mappings

import play.api.data.Mapping
import play.api.data.format.Formatter
import play.api.data.FormError
import play.api.data.Forms
import java.util.UUID

object UUIDMapping {
  implicit val uuidFormatter = new Formatter[UUID] {

    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], UUID] = {
      data.get(key).map { value =>
        try {
          Right(UUID.fromString(value).asInstanceOf[UUID])
        } catch {
          case e: IllegalArgumentException => {
            error(key, "error.required")
          }
        }
      }.getOrElse(error(key, "error.required"))
    }

    override def unbind(key: String, value: UUID): Map[String, String] = {
      Map(key -> value.toString())
    }

    private def error(key: String, msg: String) = Left(List(new FormError(key, msg)))
  }

  def uuidType: Mapping[UUID] = Forms.of[UUID]
}