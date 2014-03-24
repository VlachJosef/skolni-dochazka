package model.views.helper

import play.api.i18n.Messages

object OneTimeHelper {
  def escapeDateString(dateTime: String): String = {
    dateTime.replaceAll(":", "_").replaceAllLiterally(".", "_")
  }
}