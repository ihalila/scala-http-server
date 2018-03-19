package la.hali

import java.net.InetSocketAddress
import java.nio.channels.{AsynchronousChannelGroup, InterruptedByTimeoutException}
import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import fs2._
import fs2.io.tcp
import fs2.io.tcp.Socket

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.{Failure, Success, Try}

object HttpServer extends LazyLogging {

  def toBytes(socket: Socket[IO]): Stream[IO, Byte] = {
    socket.reads(256 * 1024, Some(FiniteDuration(1L, TimeUnit.MINUTES)))
      .onFinalize(socket.endOfOutput)
      .handleErrorWith({
        case _: InterruptedByTimeoutException =>
          logger.debug("Socket timed out.")
          Stream.empty
        case throwable: Throwable =>
          logger.warn(s"Socket error encountered: $throwable")
          Stream.empty
      })
  }

  def toRequests(byteStream: Stream[IO, Byte]): Stream[IO, Try[HttpRequest]] = {

    def pullRequests(stream: Stream[IO, Byte], byteBuffer: ArrayBuffer[Byte]): Pull[IO, Try[HttpRequest], Option[Stream[IO, Byte]]] = {
      stream.pull.unconsChunk.flatMap[IO, Try[HttpRequest], Option[Stream[IO, Byte]]] {
        case None =>
          // End of stream
          // TODO: Handle remaining bytes
          if (byteBuffer.nonEmpty) {
            logger.warn(s"Discarding ${byteBuffer.length} bytes")
          }
          Pull.pure(None)

        case Some((byteChunk, stream)) =>
          byteBuffer.appendAll(byteChunk.toVector)
          HttpRequest.fromBytes(byteBuffer) match {
            case NeedsMoreBytes =>
              // Not enough bytes to form a valid request, keep reading
              pullRequests(stream, byteBuffer)
            case Done(req, unusedBytes) =>
              Pull.output1(Success(req)) >> pullRequests(stream, unusedBytes)
            case MalformedRequest(message) =>
              Pull.output1(Failure(new Exception(message))) >> Pull.pure(None)
          }
      }
    }

    pullRequests(byteStream, ArrayBuffer()).stream
  }

  def respond(responder: PartialFunction[HttpRequest, HttpResponse], request: HttpRequest): HttpResponse = {
    if (responder.isDefinedAt(request)) {
      Try(responder.apply(request))
        .recover { case t => ServerErrorResponse(t) }
        .getOrElse(ServerErrorResponse(new Exception("Recovery failed")))
    } else {
      request match {
        case _: UnknownMethod => NotImplementedResponse
        case _ => NotFoundResponse
      }
    }
  }

  def run(responder: PartialFunction[HttpRequest, HttpResponse]): Stream[IO, (Option[HttpRequest], HttpResponse)] = {
    val executorService = Executors.newFixedThreadPool(10)
    implicit val asynchronousChannelGroup: AsynchronousChannelGroup = AsynchronousChannelGroup.withThreadPool(executorService)
    implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executorService)

    tcp.server[IO](new InetSocketAddress(8080))
      .flatMap(_.map(socket => {
        toBytes(socket)
          .through(toRequests)
          .map({
            case Failure(_) => (None, BadRequestResponse)
            case Success(req) => (Some(req), respond(responder, req))
          })
          .flatMap({ case (request, response) =>
            Stream.eval(socket.write(Chunk.array(response.toBytes))
              .map(_ => (request, response)))
          })
      })).join(10)
  }
}
