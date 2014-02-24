package model

import java.util.UUID
import org.sedis.Dress
import redis.clients.jedis.SortingParams
import redis.clients.jedis.Jedis

case class Trida(uuidTrida: Option[UUID], nazev: String)

object Trida {
  def getAll(client: Jedis): List[model.Trida] = {
    val sedis = Dress.up(client)
    val nazvyTrid = sedis.sort("tridy", new SortingParams().alpha().asc())
    nazvyTrid.map(
      nazevTridy => Trida(
        Some(UUID.fromString(sedis.hget(s"trida:$nazevTridy", "uuid"))), nazevTridy))
  }

  def getByUUID(uuidTrida: String, client: Jedis) = {
    val sedis = Dress.up(client)
    val nazev = sedis.hget(s"trida:$uuidTrida", "nazev")
    Trida(Some(UUID.fromString(uuidTrida)), nazev)
  }

  def getMaxPoradoveCisloZak(uuidTrida: String, client: Jedis): Int ={
    val zaci = Zak.getByUUIDTrida(uuidTrida, client)
    println(zaci)
    zaci.reverse match {
      case head :: xs => head.poradoveCislo + 1
      case _ => 1
    }
  }

  def deleteByUUID(uuidTrida: String, client: Jedis) = {

    val sedis = Dress.up(client)
    val nazev = sedis.hget(s"trida:$uuidTrida", "nazev")
    sedis.srem("tridy", nazev) // smazeme tridu z mnoziny trid
    client.del(s"trida:$nazev")
    client.del(s"trida:$uuidTrida")
    val zaci = sedis.smembers(uuidTrida)
    zaci.foreach(zak => Zak.deleteByUUIDZak(zak, client))
    client.del(uuidTrida)
  }
}