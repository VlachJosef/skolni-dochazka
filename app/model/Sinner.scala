package model

import java.util.Date
import redis.clients.jedis.Jedis
import org.sedis.Dress
import org.joda.time.format.DateTimeFormat
import com.github.nscala_time.time.Implicits._
import org.joda.time.LocalDate

object Sinner {
  def allSinners(client: Jedis): Set[String] = {
    val sedis = Dress.up(client)
    sedis.smembers("sinner")
  }

  def saveSinner(sinner: String, client: Jedis) = {
    val sedis = Dress.up(client)
    sedis.sadd("sinner", sinner)
  }
}