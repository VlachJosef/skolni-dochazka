package model

import java.util.UUID
import redis.clients.jedis.Jedis
import org.sedis.Dress

case class Zak(uuidZak: Option[UUID], jmeno: String, prijmeni: String, uuidTrida: UUID)

object Zak {
  def getAllByUUIDTrida(uuidTrida: String) = {

  }

  def getByUUIDZak(uuidZak: String, client: Jedis) = {
    val sedis = Dress.up(client)
    val jmeno = sedis.hget(s"zak:$uuidZak", "jmeno")
    val prijmeni = sedis.hget(s"zak:$uuidZak", "prijmeni")
    val uuidTridy = UUID.fromString(sedis.hget(s"zak:$uuidZak", "uuidTridy"))
    Zak(Some(UUID.fromString(uuidZak)), jmeno, prijmeni, uuidTridy)
  }
}