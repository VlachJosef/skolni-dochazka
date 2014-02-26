package model

import java.util.Date
import redis.clients.jedis.Jedis
import org.sedis.Dress
import org.joda.time.format.DateTimeFormat
import com.github.nscala_time.time.Implicits._
import org.joda.time.LocalDate

case class Pritomnost(uuidZak: String, pocetHodin: Int)
case class Dochazka(den: LocalDate, uuidTrida: String, hodiny: List[Pritomnost])
case class DochazkaUpdate(den: LocalDate, uuidZak: String, pocetHodin: Int)

case class DochazkaZaka(zak: Zak, pocetHodin: Int)
case class DochazkaTableHeader(zaci: List[Zak])
case class DochazkaTableBody(den: String, zaci: List[DochazkaZaka])
case class DochazkaTable(header: DochazkaTableHeader, dnyData: List[DochazkaTableBody])

object Dochazka {

  val formatter = DateTimeFormat.forPattern("dd.MM.yyyy")

  def getDochazkaTable(uuidTrida: String, client: Jedis) = {
    val sedis = Dress.up(client)
    val dnyDochazky = this.getDnyDochazkyByUUidTrida(uuidTrida, client)
    val zaci = Zak.getByUUIDTrida(uuidTrida, client);
    val dnyLocalDate = dnyDochazky.map(formatter.parseLocalDate(_)).toList.sorted
    val tableBody = for (
      den <- dnyLocalDate
    ) yield {
      val dochazkaZaka = for (
        zak <- zaci
      ) yield {
        val denStr = den.toString("dd.MM.yyyy")
        val pocetHodin = client.get(s"dochazka:$denStr:${zak.uuidZak.get.toString}")
        DochazkaZaka(zak, pocetHodin.toInt)
      }
      DochazkaTableBody(den.toString("dd.MM.yyyy"), dochazkaZaka)
    }
    DochazkaTable(DochazkaTableHeader(zaci), tableBody)
  }

  def updateDochazka(dochazkaUpdate: DochazkaUpdate, client: Jedis) = {
    val uuidZak = dochazkaUpdate.uuidZak
    val sedis = Dress.up(client)
    val den = dochazkaUpdate.den.toString("dd.MM.yyyy")

    val exists = client.exists(s"dochazka:$den:$uuidZak")

    if (exists) {
      client.set(s"dochazka:$den:$uuidZak", dochazkaUpdate.pocetHodin.toString)
    } else {
      println("ERROR, pokus o update neexistujiciho zaznamu")
    }
  }

  def saveDochazka(dochazka: Dochazka, client: Jedis) = {
    val uuidTrida = dochazka.uuidTrida
    val sedis = Dress.up(client)
    val den = dochazka.den.toString("dd.MM.yyyy")

    if (!sedis.sismember(s"dny:$uuidTrida", den)) {
      sedis.sadd(s"dny:$uuidTrida", den)
    }
    val doc = dochazka.hodiny
    doc.map(pritomnost => {
      val uuidZak = pritomnost.uuidZak
      client.set(s"dochazka:$den:${pritomnost.uuidZak}", pritomnost.pocetHodin.toString)
    })
  }

  def getDnyDochazkyByUUidTrida(uuidTrida: String, client: Jedis): Set[String] = {
    val sedis = Dress.up(client)
    sedis.smembers(s"dny:$uuidTrida")
  }

  def deleteDay(deleteDay: (LocalDate, String), client: Jedis) = {
    val den = deleteDay._1.toString("dd.MM.yyyy")
    val uuidTrida = deleteDay._2
    val sedis = Dress.up(client)

    val ismember = sedis.sismember(s"dny:$uuidTrida", den)
    if (ismember) {
      sedis.srem(s"dny:$uuidTrida", den)
    } else {
      println("ERROR, pokus o delete neexistujiciho zaznamu")
    }
  }
}