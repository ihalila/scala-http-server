package la.hali

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

sealed trait HttpRequest {
  def path: String
  def headers: Headers
}

final case class Get(override val path: String, override val headers: Headers) extends HttpRequest {
  override def toString: String = s"GET: $path"
}
final case class Post(override val path: String, override val headers: Headers, body: Array[Byte]) extends HttpRequest {
  override def toString: String = s"POST: $path [${body.length} bytes]"
}
final case class UnknownMethod(override val path: String, override val headers: Headers) extends HttpRequest

sealed trait RequestParsingResult
final case class Done(request: HttpRequest, remainingBytes: ArrayBuffer[Byte]) extends RequestParsingResult
case object NeedsMoreBytes extends RequestParsingResult
final case class MalformedRequest(message: String) extends RequestParsingResult

object HttpRequest {
  /** Attempt to construct an HttpRequest from the given bytes. If successful returns the request
    * and any remaining unconsumed bytes
    */
  def fromBytes(bytes: ArrayBuffer[Byte]): RequestParsingResult = {
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
        (new String(key.toArray).toLowerCase, new String(value.drop(1).toArray).trim)
      }

      def readHeaders(chars: ArrayBuffer[Char], headers: Headers): (Headers, ArrayBuffer[Char]) = {
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
      val (headers, remaining) = readHeaders(tail, Headers()) match { case (h, r) => (h, r.map(_.toByte)) }

      // Body // TODO: Support Transfer-Encoding
      val contentLength = headers.get("Content-Length").map(cl => Try(cl.toInt).getOrElse(-1)) // Use -1 to represent a malformed value
      val bodyLength = contentLength.getOrElse(0) // No header means no body

      if (remaining.length < bodyLength) {
        NeedsMoreBytes
      } else if (bodyLength < 0) {
        MalformedRequest(s"Malformed Content-Length: $bodyLength")
      } else {

        val (body, tail) = remaining.splitAt(bodyLength)

        method match {
          case "GET" => Done(Get(target, headers), tail)
          case "POST" => Done(Post(target, headers, body.toArray), tail)
          case _ => Done(UnknownMethod(target, headers), tail)
        }
      }
    } else {
      // Headers not completely available yet
      NeedsMoreBytes
    }
  }
}