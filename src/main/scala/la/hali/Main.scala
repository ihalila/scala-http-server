package la.hali

import com.typesafe.scalalogging.LazyLogging

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info("Starting HTTP server")
    HttpServer.run
      .attempt
      .map({
        case Left(e) => logger.warn(s"Failed to handle request: $e")
        case Right((socket, req)) =>
          logger.info(s"Got request: $req")
      })
      .repeat
      .compile
      .drain
      .unsafeRunSync()
  }
}
