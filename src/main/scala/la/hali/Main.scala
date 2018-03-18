package la.hali

import com.typesafe.scalalogging.LazyLogging

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info("Starting HTTP server")
    HttpServer.run
      .attempt
      .map({
        case Left(e) => logger.warn(s"Failed to handle request: $e")
        case Right((req, res)) =>
          logger.info(s"Got request: $req")
          logger.info(s"Responded with $res")
      })
      .compile
      .drain
      .unsafeRunSync()
  }
}
