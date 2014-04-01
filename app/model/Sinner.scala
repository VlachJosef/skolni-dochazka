package model

import controllers.Application._
import java.util.Date
import redis.clients.jedis.Jedis
import org.sedis.Dress
import org.joda.time.format.DateTimeFormat
import com.github.nscala_time.time.Implicits._
import org.joda.time.LocalDate

object Sinner {
  def allSinners(implicit client: Jedis): Set[String] = {
    withSedis { _.smembers("sinner") }
  }

  def saveSinner(sinner: String)(implicit client: Jedis) = {
    withSedis { _.sadd("sinner", sinner) }
  }
}