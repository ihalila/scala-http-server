package la.hali

import cats.effect.IO
import fs2.io.tcp.Socket

import scala.collection.mutable.ArrayBuffer

case class SocketRequest(socket: Socket[IO], request: HttpRequest)

sealed trait HttpRequest {
  def path: String
  def headers: Map[String, String]
}

case class GETRequest(override val path: String, override val headers: Map[String, String]) extends HttpRequest

object HttpRequest {
  /** Attempt to construct an HttpRequest from the given bytes. If successful returns the request
    * and any remaining unconsumed bytes
    */
  def fromBytes(bytes: ArrayBuffer[Byte]): Option[(HttpRequest, ArrayBuffer[Byte])] = {
    // Convert to chars for reading headers
    val chars = bytes.map(_.toChar)
    if (chars.indexOfSlice("\r\n\r\n") >= 0) {
      // All headers are available to be read

      // Request line
      val (line, tail) = chars.span(_ != '\r') match { case (l, t) => (l, t.drop(2)) } // Drop \r\n
      val requestLine = new String(line.toArray)

      val methodSeparator = requestLine.indexOf(' ')
      val method = requestLine.slice(0, methodSeparator)

      val targetSeparator = requestLine.indexOf(' ', methodSeparator + 1)
      val target = requestLine.slice(methodSeparator + 1, targetSeparator)

      //val httpVersion = requestLine.slice(targetSeparator + 1, requestLine.length)

      // Headers
      def toHeader(chars: ArrayBuffer[Char]): (String, String) = {
        val (key, value) = chars.span(_ != ':')
        (new String(key.toArray), new String(value.drop(1).toArray).trim)
      }

      def readHeaders(chars: ArrayBuffer[Char], headers: Map[String, String]): (Map[String, String], ArrayBuffer[Char]) = {
        if (chars.isEmpty) {
          (headers, chars)
        } else {
          val (h, tail) = chars.span(_ != '\r') match { case (x, t) => (x, t.drop(2)) } // Drop \r\n
          if (h.isEmpty) {
            // Empty line means headers have ended
            (headers, tail)
          } else {
            val header = toHeader(h)
            readHeaders(tail, headers + header)
          }
        }
      }

      // Convert back to bytes for reading the body
      val (headers, remaining) = readHeaders(tail, Map()) match { case (h, r) => (h, r.map(_.toByte)) }

      // Body

      method match {
        case "GET" => Some((GETRequest(target, headers), remaining))
      }

    } else {
      // Headers not available yet
      None
    }
  }
}