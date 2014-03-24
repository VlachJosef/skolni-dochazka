package model

import redis.clients.jedis.Jedis
import org.sedis.Dress

case class SlozeniTridy(trida: Trida, zaci: List[Zak])
case class Skola(slozeniTrid: List[SlozeniTridy])

object Skola {

  def getSkola(client: Jedis) = {
    val sedis = Dress.up(client)
    val tridy = Trida.getAll(client)

    val SlozeniTrid = tridy.map(trida => {
      val zaciUuid = sedis.smembers(trida.uuidTrida.get.toString)
      val zaci = zaciUuid.map(uuidZak => {
        Zak.getByUUIDZak(uuidZak, client)
      })
      val zaciSorted = zaci.toList.sortBy(zak => zak.prijmeni)
      SlozeniTridy(trida, zaciSorted)
    })
    Skola(SlozeniTrid)
  }

  def getSlozeniTridy(uuidTrida: String, client: Jedis) = {
    val trida = Trida.getByUUID(uuidTrida, client)
    val zaci = Zak.getByUUIDTrida(uuidTrida, client)
    SlozeniTridy(trida, zaci)
  }
}