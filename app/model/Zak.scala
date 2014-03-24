package model

import java.util.UUID
import redis.clients.jedis.Jedis
import org.sedis.Dress

case class Zak(uuidZak: Option[UUID], jmeno: String, prijmeni: String, uuidTrida: UUID)

object Zak {

  def getByUUIDTrida(uuidTrida: String, client: Jedis): Set[Zak] =  {
    val sedis = Dress.up(client)
    val zaci = sedis.smembers(uuidTrida)
    zaci.map(uuidZak => getByUUIDZak(uuidZak, client))
  }

  def getByUUIDZak(uuidZak: String, client: Jedis): Zak = {
    val sedis = Dress.up(client)
    val jmeno = sedis.hget(s"zak:$uuidZak", "jmeno")
    val prijmeni = sedis.hget(s"zak:$uuidZak", "prijmeni")
    val uuidTridy = UUID.fromString(sedis.hget(s"zak:$uuidZak", "uuidTridy"))
    Zak(Some(UUID.fromString(uuidZak)), jmeno, prijmeni, uuidTridy)
  }

  def deleteByUUIDZak(uuidZak: String, client: Jedis) = {
    val sedis = Dress.up(client)
    sedis.srem("zaci", uuidZak)
    client.del(s"zak:$uuidZak")
  }
}