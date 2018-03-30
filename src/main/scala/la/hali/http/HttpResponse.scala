package la.hali.http

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

abstract class HttpResponse(val statusLine: String, val headers: Headers, val body: Array[Byte]) {
  type Self <: HttpResponse

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def copy(statusLine: String = this.statusLine, headers: Headers = this.headers, body: Array[Byte] = this.body): Self

  def toBytes: Array[Byte] =
    new StringBuilder(HttpResponse.statusLine(statusLine))
      .append(headers.toString())
      .append("\r\n")
      .toString()
      .getBytes(StandardCharsets.US_ASCII) ++ body
}

object HttpResponse {
  def statusLine(status: String): String = s"HTTP/1.1 $status\r\n"

  def headers: Headers = {
    val date = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneId.of("UTC")).format(Instant.now())
    Headers(Map(
      "Date" -> date,
      "Server" -> "la.hali.scala-http-server"
    ))
  }

  def contentHeads(body: String): Headers = Headers(Map(
    "Content-Length" -> s"${body.getBytes(StandardCharsets.US_ASCII).length}",
    "Content-Type" -> "text/plain"
  ))
}

class NotImplemented private(statusLine: String, headers: Headers, body: Array[Byte]) extends HttpResponse(statusLine, headers, body) {
  override type Self = NotImplemented

  override def copy(statusLine: String, headers: Headers, body: Array[Byte]): NotImplemented =
    new NotImplemented(statusLine, headers, body)
}

object NotImplemented {
  val response: NotImplemented = {
    val message: String = "501 - Not Implemented"
    new NotImplemented(
      "501 Not Implemented",
      HttpResponse.headers ++ HttpResponse.contentHeads(message),
      message.getBytes(StandardCharsets.US_ASCII))
  }
}


class BadRequest private(statusLine: String, headers: Headers, body: Array[Byte]) extends HttpResponse(statusLine, headers, body) {
  override type Self = BadRequest

  override def copy(statusLine: String, headers: Headers, body: Array[Byte]): BadRequest =
    new BadRequest(statusLine, headers, body)
}

object BadRequest {
  val response: BadRequest = {
    val message: String = "400 - Bad Request"
    new BadRequest(
      "400 Bad Request",
      HttpResponse.headers ++ HttpResponse.contentHeads(message),
      message.getBytes(StandardCharsets.US_ASCII))
  }
}

class NotFound private(statusLine: String, headers: Headers, body: Array[Byte]) extends HttpResponse(statusLine, headers, body) {
  override type Self = NotFound

  override def copy(statusLine: String, headers: Headers, body: Array[Byte]): NotFound =
    new NotFound(statusLine, headers, body)
}

object NotFound {
  val response: NotFound = {
    val message: String = "404 - Not Found"
    new NotFound(
      "404 Not Found",
      HttpResponse.headers ++ HttpResponse.contentHeads(message),
      message.getBytes(StandardCharsets.US_ASCII))
  }
}

class ServerError private(statusLine: String, headers: Headers, body: Array[Byte]) extends HttpResponse(statusLine, headers, body) {
  override type Self = ServerError

  override def copy(statusLine: String, headers: Headers, body: Array[Byte]): ServerError =
    new ServerError(statusLine, headers, body)
}

object ServerError {
  def response(throwable: Throwable): ServerError = {
    val message: String = s"500 - Internal Server Error\n$throwable"
    new ServerError(
      "500 Internal Server Error",
      HttpResponse.headers ++ HttpResponse.contentHeads(message),
      message.getBytes(StandardCharsets.US_ASCII))
  }
}

class OK private(statusLine: String, headers: Headers, body: Array[Byte]) extends HttpResponse(statusLine, headers, body) {
  override type Self = OK

  override def copy(statusLine: String, headers: Headers, body: Array[Byte]): OK =
    new OK(statusLine, headers, body)
}

object OK {
  def response(content: String): OK = {
    new OK(
      "200 OK",
      HttpResponse.headers ++ HttpResponse.contentHeads(content),
      content.getBytes(StandardCharsets.US_ASCII))
  }
}