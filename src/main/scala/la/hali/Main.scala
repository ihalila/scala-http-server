package la.hali

import com.typesafe.scalalogging.LazyLogging
import fs2.Chunk

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info("Starting HTTP server")
    HttpServer.run
      .map(req => {
        logger.info(s"Got request: ${req._2}")
      })
      .compile
      .drain
      .unsafeRunSync()
  }
}
