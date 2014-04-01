package model

import controllers.Application._
import redis.clients.jedis.Jedis
import org.sedis.Dress

case class SlozeniTridy(trida: Trida, zaci: List[Zak])
case class Skola(slozeniTrid: List[SlozeniTridy])

object Skola {

  def getSkola(implicit client: Jedis) = {
    withSedis { sedis =>
      val tridy = Trida.getAll

      val SlozeniTrid = tridy.map(trida => {
        val zaciUuid = sedis.smembers(trida.uuidTrida.get.toString)
        val zaci = zaciUuid.map(uuidZak => {
          Zak.getByUUIDZak(uuidZak)
        })
        val zaciSorted = zaci.toList.sortBy(zak => zak.poradoveCislo)
        SlozeniTridy(trida, zaciSorted)
      })
      Skola(SlozeniTrid)
    }
  }

  def getSlozeniTridy(uuidTrida: String)(implicit client: Jedis) = {
    val trida = Trida.getByUUID(uuidTrida)
    val zaci = Zak.getByUUIDTrida(uuidTrida)
    SlozeniTridy(trida, zaci)
  }
}