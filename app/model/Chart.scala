package model

import java.util.Date
import redis.clients.jedis.Jedis
import org.sedis.Dress
import org.joda.time.format.DateTimeFormat
import com.github.nscala_time.time.Implicits._
import org.joda.time.LocalDate

case class Chart(zak: Zak, dnyData: String, dochazkaData: String, kumulativniAbsenceData: String, absence: Double, zameskaneHodiny: Int)

object Chart {

  val formatter = DateTimeFormat.forPattern("dd.MM.yyyy")

  def get(uuidTrida: String, client: Jedis) = {
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
          val absence = (((zameskaneHodiny.toDouble * 100.0) / (dochazkaList.size.toDouble * 4.0) * 100.0).toInt / 100.0)
          (absence, zameskaneHodiny)
        case _ => (0.0, 0)
      }
      Chart(zak, dnyList.mkString(","), dochazkaList.mkString(","), kumulativniAbsenceData.mkString(","), absenceAndHodiny._1, absenceAndHodiny._2)
    })
    chartsData
  }

}