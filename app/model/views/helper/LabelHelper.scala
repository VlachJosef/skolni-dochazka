package model.views.helper

import play.api.i18n.Messages

object LabelHelper {
  def apply(fieldName: String, label: String = ""): String = {
    if (!label.isEmpty) label
    if (Messages.isDefinedAt(fieldName)) {
      Messages(fieldName)
    } else {
      val lastIndex = fieldName.lastIndexOf(".");
      if (lastIndex != -1) {
        val shortFieldName = fieldName.substring(lastIndex + 1)
        if (Messages.isDefinedAt(shortFieldName)) {
          Messages(shortFieldName)
        } else {
          fieldName
        }
      } else {
        fieldName
      }
    }
  }
}