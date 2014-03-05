package model

import java.util.UUID
import redis.clients.jedis.Jedis
import org.sedis.Dress

case class Zak(uuidZak: Option[UUID], jmeno: String, prijmeni: String, poradoveCislo: Int, aktivity: Boolean, uuidTrida: UUID)

object Zak {

  def exists(uuidZak: String, client: Jedis): Boolean = {
    val sedis = Dress.up(client)
    sedis.sismember("zaci", uuidZak)
  }

  def save(zak: Zak, client: Jedis) = {
    val uuidZak = UUID.randomUUID.toString
    val sedis = Dress.up(client)

    sedis.sadd("zaci", uuidZak)

    sedis.hset(s"zak:$uuidZak", "jmeno", zak.jmeno)
    sedis.hset(s"zak:$uuidZak", "prijmeni", zak.prijmeni)
    sedis.hset(s"zak:$uuidZak", "uuidTridy", zak.uuidTrida.toString)
    sedis.hset(s"zak:$uuidZak", "poradoveCislo", zak.poradoveCislo.toString)
    sedis.hset(s"zak:$uuidZak", "aktivity", zak.aktivity.toString)

    sedis.sadd(zak.uuidTrida.toString, uuidZak) // pridame do tridy uuidTrida zaka s uuidZak

  }

  def update(zak: Zak, client: Jedis) = {
    val uuidZak = zak.uuidZak.get.toString

    val sedis = Dress.up(client)

    if (sedis.sismember("zaci", zak.uuidZak.get.toString)) {
      sedis.hset(s"zak:$uuidZak", "jmeno", zak.jmeno)
      sedis.hset(s"zak:$uuidZak", "prijmeni", zak.prijmeni)
      sedis.hset(s"zak:$uuidZak", "poradoveCislo", zak.poradoveCislo.toString)
      sedis.hset(s"zak:$uuidZak", "aktivity", zak.aktivity.toString)

      val oldUUIDTridy = sedis.hget(s"zak:$uuidZak", "uuidTridy")
      val uuidTrida = zak.uuidTrida.toString
      if (oldUUIDTridy != uuidTrida) {
        sedis.hset(s"zak:$uuidZak", "uuidTridy", uuidTrida)
        sedis.srem(oldUUIDTridy, uuidZak) // presuneme zaka z puvodni tridy...
        sedis.sadd(uuidTrida, uuidZak) // ... do nove tridy
      }
    }
  }

  def getByUUIDTrida(uuidTrida: String, client: Jedis): List[Zak] = {
    val sedis = Dress.up(client)
    val uuidZaku = sedis.smembers(uuidTrida)
    val zaci = uuidZaku.map(uuidZak => getByUUIDZak(uuidZak, client))
    zaci.toList.sortBy(zak => zak.poradoveCislo)
  }

  def getByUUIDZak(uuidZak: String, client: Jedis): Zak = {
    val sedis = Dress.up(client)
    val jmeno = sedis.hget(s"zak:$uuidZak", "jmeno")
    val prijmeni = sedis.hget(s"zak:$uuidZak", "prijmeni")
    val poradoveCislo = sedis.hget(s"zak:$uuidZak", "poradoveCislo")
    val aktivity = sedis.hget(s"zak:$uuidZak", "aktivity")
    val uuidTridy = UUID.fromString(sedis.hget(s"zak:$uuidZak", "uuidTridy"))
    Zak(Some(UUID.fromString(uuidZak)), jmeno, prijmeni, poradoveCislo.toInt, aktivity.toBoolean, uuidTridy)
  }

  def deleteByUUIDZak(uuidZak: String, client: Jedis): Zak = {
    val sedis = Dress.up(client)
    val zak = getByUUIDZak(uuidZak, client)
    val uuidTrida = sedis.hget(s"zak:$uuidZak", "uuidTridy")
    sedis.srem(uuidTrida, uuidZak)
    sedis.srem("zaci", uuidZak)
    client.del(s"zak:$uuidZak")
    zak
  }
}