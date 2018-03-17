package la.hali

import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import fs2._
import fs2.io.tcp
import fs2.io.tcp.Socket

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object HttpServer extends LazyLogging {

  def toBytes(socket: Socket[IO]): Stream[IO, Byte] = {
    socket.reads(256 * 1024, Some(FiniteDuration(1L, TimeUnit.MINUTES)))
  }

  def toRequests(byteStream: Stream[IO, Byte]): Stream[IO, Either[MalformedRequest, HttpRequest]] = {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def pullRequests(stream: Stream[IO, Byte], byteBuffer: ArrayBuffer[Byte]): Pull[IO, Either[MalformedRequest, HttpRequest], Option[Stream[IO, Byte]]] = {
      stream.pull.unconsChunk.flatMap {
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
            case None =>
              // Not enough bytes to form a valid request, keep reading
              pullRequests(stream, byteBuffer)
            case Some((req, unusedBytes)) =>
              Pull.output1(Right(req)) >> pullRequests(stream, unusedBytes)
          }
      }
    }

    pullRequests(byteStream, ArrayBuffer()).stream
  }

  def run: Stream[IO, (Socket[IO], Either[MalformedRequest, HttpRequest])] = {
    val executorService = Executors.newFixedThreadPool(10)
    implicit val asynchronousChannelGroup: AsynchronousChannelGroup = AsynchronousChannelGroup.withThreadPool(executorService)
    implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executorService)

    for {
      sockets <- tcp.server[IO](new InetSocketAddress(8080))
      socket <- sockets
      bytes = HttpServer.toBytes(socket)
      req <- HttpServer.toRequests(bytes)
    } yield (socket, req)
  }
}
