package model

import java.util.Date
import redis.clients.jedis.Jedis
import org.joda.time.format.DateTimeFormat
import com.github.nscala_time.time.Implicits._
import org.joda.time.LocalDate

object FondHodin {
  val vyuka: Int = 4
  val aktivity: Int = 2

  def pocetHodinByTypVyuky(typVyuky: String): Int = {
    typVyuky match {
      case "vyuka" => FondHodin.vyuka
      case "aktivity" => FondHodin.aktivity
      case _ => 0
    }
  }
}

case class DenniDochazka(den: LocalDate, hodinyVyuka: Int = -1, hodinyAktivity: Int = -1)
case class VyucovaniAbsence(pocetDnu: Int, pritomenHodin: Int, fondHodin: Int) {
  val pocetHodin: Int = pocetDnu * fondHodin
  val zameskaneHodiny = pocetHodin - pritomenHodin
  val absence: Double = {
    ((zameskaneHodiny.toDouble * 100.0) / pocetHodin.toDouble * 100.0).toInt / 100.0
  }
}

case class SummaryAbsence(summaryVyuka: VyucovaniAbsence, summaryAktivity: VyucovaniAbsence) {
  val pocetHodinyTotal: Int = summaryVyuka.pocetHodin + summaryAktivity.pocetHodin
  val zameskaneHodinyTotal: Int = summaryVyuka.zameskaneHodiny + summaryAktivity.zameskaneHodiny
  val absenceTotal: Double = {
    ((zameskaneHodinyTotal.toDouble * 100.0) / pocetHodinyTotal.toDouble * 100.0).toInt / 100.0
  }
}
case class Chart(zak: Zak, dnyData: String, dochazkaData: String, dnyAktivityData: String, kumulativniAbsenceData: String, summaryAbsence: SummaryAbsence)

object Chart {
  type DenPocetHodin = List[(String, Int)]

  val formatter = DateTimeFormat.forPattern("dd.MM.yyyy")

  def get(uuidTrida: String)(implicit client: Jedis): List[model.Chart] = {

    def getPocetHodinZaDen(zak: Zak, dny: List[String], typVyuky: String): DenPocetHodin = {
      for (
        den <- dny.toList
      ) yield {
        val pocetHodin = client.get(s"$typVyuky:$den:${zak.uuidZak.get.toString}")
        (den, pocetHodin.toInt)
      }
    }

    def convertDenPocetHodinToDenniDochazka(vyuka: DenPocetHodin, aktivity: DenPocetHodin): List[DenniDochazka] = {
      val mapVyuka = vyuka.map({ v =>
        v._1 -> DenniDochazka(formatter.parseLocalDate(v._1), hodinyVyuka = v._2.toInt)
      }).toMap

      val mapAktivity = aktivity.map({ v =>
        val key = v._1
        val hodinyAktivityValue = v._2.toInt
        mapVyuka.contains(key) match {
          case true => {
            val record = mapVyuka.get(key).get
            key -> DenniDochazka(record.den, record.hodinyVyuka, hodinyAktivityValue)
          }
          case _ => key -> DenniDochazka(formatter.parseLocalDate(key), hodinyAktivity = hodinyAktivityValue)
        }
      }).toMap

      val mapUnion = mapAktivity ++ mapVyuka.filter(x => !mapAktivity.contains(x._1))

      mapUnion.values.toList.sortBy(_.den)
    }

    val zaci = Zak.getByUUIDTrida(uuidTrida);
    val dnyVyuky = Dochazka.getDnyDochazkyByUUidTrida(uuidTrida, "vyuka").toList.sortBy(formatter.parseLocalDate(_))
    val dnyAktivity = Dochazka.getDnyDochazkyByUUidTrida(uuidTrida, "aktivity").toList.sortBy(formatter.parseLocalDate(_))

    val chartsData = zaci.map(zak => {
      val denPocetHodinVyuka = getPocetHodinZaDen(zak, dnyVyuky, "vyuka")
      val denPocetHodinAktivity = if (zak.aktivity) {
        getPocetHodinZaDen(zak, dnyAktivity, "aktivity")
      } else {
        List[Tuple2[String, Int]]()
      }

      val dochazkaDenList = convertDenPocetHodinToDenniDochazka(denPocetHodinVyuka, denPocetHodinAktivity);

      val dnyList = dochazkaDenList.map("'" + _.den.toString("dd.MM.yyyy") + "'")
      val dnyVyukaList = dochazkaDenList.map(_.hodinyVyuka match {
        case -1 => null
        case x => x
      })
      val dnyAktivityList = dochazkaDenList.map(_.hodinyAktivity match {
        case -1 => null
        case x => x
      })
      val dochazkaList = denPocetHodinVyuka.map(_._2.toInt).toList

      val kumulativniAbsenceData = for (i <- 1 to dochazkaDenList.size) yield {
        dochazkaDenList.take(i).foldLeft(0)((acc, d) => acc + (d match {
          case DenniDochazka(_, -1, aktivity) => FondHodin.aktivity - aktivity
          case DenniDochazka(_, vyuka, -1) => FondHodin.vyuka - vyuka
          case DenniDochazka(_, vyuka, aktivity) => FondHodin.vyuka + FondHodin.aktivity - vyuka - aktivity
        }))
      }

      val vyukaSummary = VyucovaniAbsence(denPocetHodinVyuka.size, denPocetHodinVyuka.foldLeft(0)(_ + _._2), FondHodin.vyuka)
      val aktivitySummary = VyucovaniAbsence(denPocetHodinAktivity.size, denPocetHodinAktivity.foldLeft(0)(_ + _._2), FondHodin.aktivity)

      val summaryAbsence = SummaryAbsence(vyukaSummary, aktivitySummary)

      Chart(zak, dnyList.mkString(","), dnyVyukaList.mkString(","), dnyAktivityList.mkString(","), kumulativniAbsenceData.mkString(","), summaryAbsence)
    })
    chartsData
  }
}