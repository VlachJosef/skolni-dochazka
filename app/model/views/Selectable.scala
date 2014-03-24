package model.views

case class Selectable(
  kod: String,
  value: String)

object Selectable {
  def apply[A](xs: Seq[A])(f: A => (String, String)): Seq[Selectable] = {
    if (xs.isEmpty) {
      Seq(Selectable("", ""));
    } else {
      xs map { x =>
        val (key, value) = f(x)
        Selectable(key, value)
      }
    }
  }
}