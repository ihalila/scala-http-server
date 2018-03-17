package la.hali

import com.typesafe.scalalogging.LazyLogging

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info("Starting HTTP server")
    val server = new HttpServer
  }
}
