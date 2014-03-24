package model.views.helper

import play.api.i18n.Messages
import play.api.data.FormError

object ErrorHelper {
  def apply(error: FormError): String = {
    Messages(error.message, LabelHelper(error.key), error.args)
  }
}