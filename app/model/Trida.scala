package model

import java.util.UUID
import org.sedis.Dress
import redis.clients.jedis.SortingParams
import redis.clients.jedis.Jedis

case class Trida(uuidTrida: UUID, nazevTridy: String)

object Trida {
  def getAll(client: Jedis): List[model.Trida] = {
    val sedis = Dress.up(client)
    val nazvyTrid = sedis.sort("tridy", new SortingParams().alpha().asc())
    nazvyTrid.map(
      nazevTridy => Trida(
        UUID.fromString(sedis.hget(s"trida:$nazevTridy", "uuid")),
        nazevTridy))
  }
}