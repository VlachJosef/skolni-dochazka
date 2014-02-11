package model

import redis.clients.jedis.Jedis
import org.sedis.Dress

case class SlozeniTridy(trida: Trida, zaci: Set[Zak])
case class Skola(slozeniTrid: List[SlozeniTridy])

object Skola {

  def getSkola(client: Jedis) = {
    val tridy = Trida.getAll(client)
    val sedis = Dress.up(client)

    val SlozeniTrid = tridy.map(trida => {
      val zaciUuid = sedis.smembers(trida.uuidTrida.get.toString)
      val zaci = zaciUuid.map(uuidZak => {
        Zak.getByUUIDZak(uuidZak, client)
      })
      SlozeniTridy(trida, zaci)
    })
    Skola(SlozeniTrid)
  }
}