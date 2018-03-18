package la.hali

import com.typesafe.scalalogging.LazyLogging
import fs2.Chunk

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info("Starting HTTP server")
    HttpServer.run
      .attempt
      .map({
        case Left(e) => logger.warn(s"Failed to handle request: $e")
        case Right(req) =>
          logger.info(s"Got request: $req")
          req.socket.write(Chunk.array(HttpResponse.error("foo"))).unsafeRunSync()
      })
      .repeat
      .compile
      .drain
      .unsafeRunSync()
  }
}
