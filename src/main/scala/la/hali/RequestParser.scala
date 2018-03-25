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

object RequestParser {
  def beginParsing(bytes: ArrayBuffer[Byte]): RequestParser =
    ParsingRequestLine(ArrayBuffer()).append(bytes)
}

sealed trait RequestParser {
  def append(bytes: Iterable[Byte]): RequestParser
}

case class ParsingRequestLine(bytes: ArrayBuffer[Byte]) extends RequestParser {
  override def append(newBytes: Iterable[Byte]): RequestParser = {
    bytes.appendAll(newBytes)
    val chars = bytes.map(_.toChar)
    if (chars.indexOfSlice("\r\n") < 0) {
      ParsingRequestLine(bytes)
    } else {
      // Request line
      val (line, tail) = chars.span(_ != '\r') match {
        case (l, t) => (l, t.drop(2))
      } // Drop \r\n
      val requestLine = new String(line.toArray)

      val methodSeparator = requestLine.indexOf(' ')
      val method = requestLine.slice(0, methodSeparator)

      val targetSeparator = requestLine.indexOf(' ', methodSeparator + 1)
      val target = requestLine.slice(methodSeparator + 1, targetSeparator)

      val httpVersion = requestLine.slice(targetSeparator + 1, requestLine.length)

      ParsingHeaders(method, target, httpVersion, Headers(), tail).append(ArrayBuffer())
    }
  }
}

case class ParsingHeaders(method: String, target: String, httpVersion: String, headers: Headers, chars: ArrayBuffer[Char]) extends RequestParser {

  override def append(newBytes: Iterable[Byte]): RequestParser = {
    chars.appendAll(newBytes.map(_.toChar))
    if (chars.indexOfSlice("\r\n") < 0) {
      // Not a full line available yet
      ParsingHeaders(method, target, httpVersion, headers, chars)
    } else {
      // A new header is available
      val (h, tail) = chars.span(_ != '\r') match {
        case (x, t) => (x, t.drop(2))
      } // Drop \r\n
      if (h.isEmpty) { // Empty line means end of headers
        val contentLength = headers.get("Content-Length").map(cl => Try(cl.toInt).getOrElse(-1)) // Use -1 to represent a malformed value
        val bodyLength = contentLength.getOrElse(0) // No header means no body
        val tailBytes = tail.map(_.toByte)
        if (bodyLength > 0) {
          ParsingBody(method, target, httpVersion, headers, tailBytes).append(ArrayBuffer())
        } else if (bodyLength == 0) {
          Done(method match {
            case "GET" => Get(target, headers)
            case "POST" => Post(target, headers, Array())
            case _ => UnknownMethod(target, headers)
          }, tailBytes)
        } else {
          MalformedRequest(s"Malformed Content-Length: $bodyLength")
        }
      } else { // Actual header
        val (key, value) = h.span(_ != ':')
        val newHeaders = headers + ((new String(key.toArray), new String(value.drop(1).toArray).trim))
        ParsingHeaders(method, target, httpVersion, newHeaders, tail).append(ArrayBuffer())
      }
    }
  }
}

case class ParsingBody(method: String, target: String, httpVersion: String, headers: Headers, bytes: ArrayBuffer[Byte]) extends RequestParser {
  private val contentLength = headers.get("Content-Length").map(cl => Try(cl.toInt).getOrElse(-1)) // Use -1 to represent a malformed value
  private val bodyLength = contentLength.getOrElse(0) // No header means no body

  override def append(newBytes: Iterable[Byte]): RequestParser = {
    bytes.appendAll(newBytes)
    if (bytes.length >= bodyLength) {
      val (bodyBytes, tailBytes) = bytes.splitAt(bodyLength)
      Done(method match {
        case "GET" => Get(target, headers)
        case "POST" => Post(target, headers, bodyBytes.toArray)
        case _ => UnknownMethod(target, headers)
      }, tailBytes)
    } else {
      ParsingBody(method, target, httpVersion, headers, bytes)
    }
  }
}

final case class Done(request: HttpRequest, remainingBytes: ArrayBuffer[Byte]) extends RequestParser {
  override def append(bytes: Iterable[Byte]): RequestParser =
    RequestParser.beginParsing(remainingBytes ++ bytes)
}

final case class MalformedRequest(message: String) extends RequestParser {
  override def append(bytes: Iterable[Byte]): RequestParser = this
}
