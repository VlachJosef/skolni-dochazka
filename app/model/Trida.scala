package model

import controllers.Application._
import java.util.UUID
import org.sedis.Dress
import redis.clients.jedis.SortingParams
import redis.clients.jedis.Jedis

case class Trida(uuidTrida: Option[UUID], nazev: String)

object Trida {
  def getAll(implicit client: Jedis): List[model.Trida] = {
    withSedis { sedis =>
      val nazvyTrid = sedis.sort("tridy", new SortingParams().alpha().asc())
      nazvyTrid.map(
        nazevTridy => Trida(
          Some(UUID.fromString(sedis.hget(s"trida:$nazevTridy", "uuid"))), nazevTridy))
    }
  }

  def getByUUID(uuidTrida: String)(implicit client: Jedis) = {
    withSedis { sedis =>
      val nazev = sedis.hget(s"trida:$uuidTrida", "nazev")
      Trida(Some(UUID.fromString(uuidTrida)), nazev)
    }
  }

  def getMaxPoradoveCisloZak(uuidTrida: String)(implicit client: Jedis): Int = {
    val zaci = Zak.getByUUIDTrida(uuidTrida)
    zaci.reverse match {
      case head :: xs => head.poradoveCislo + 1
      case _ => 1
    }
  }

  def deleteByUUID(uuidTrida: String)(implicit client: Jedis) = {
    withSedis { sedis =>
      val nazev = sedis.hget(s"trida:$uuidTrida", "nazev")
      sedis.srem("tridy", nazev) // smazeme tridu z mnoziny trid
      client.del(s"trida:$nazev")
      client.del(s"trida:$uuidTrida")
      val zaci = sedis.smembers(uuidTrida)
      zaci.foreach(zak => Zak.deleteByUUIDZak(zak))
      client.del(uuidTrida)
    }
  }
}