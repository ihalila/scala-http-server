package la.hali.http

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

trait HttpResponse {
  def toBytes: Array[Byte]
}

object HttpResponse {
  def defaultHeaders: String = {
    val date = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneId.of("UTC")).format(Instant.now())
    s"Date: $date\r\n" +
      "Server: la.hali.scala-http-server\r\n"
  }

  def contentHeaders(body: String): String = {
    s"Content-Length: ${body.getBytes(StandardCharsets.US_ASCII).length}\r\n" +
      "Content-Type: text/plain\r\n"
  }

  def toBytes(response: String, message: Option[String]): Array[Byte] = {
    new StringBuilder(s"HTTP/1.1 $response\r\n")
      .append(HttpResponse.defaultHeaders)
      .append(message.map(contentHeaders).getOrElse(""))
      .append("\r\n")
      .append(message.getOrElse(""))
      .toString()
      .getBytes(StandardCharsets.US_ASCII)
  }
}

object NotImplementedResponse extends HttpResponse {
  def toBytes: Array[Byte] =
    HttpResponse.toBytes("501 Not Implemented", Some("501 - Not Implemented"))
}

object BadRequestResponse extends HttpResponse {
  def toBytes: Array[Byte] =
    HttpResponse.toBytes("404 Bad Request", Some("400 - Bad Request"))
}

object NotFoundResponse extends HttpResponse {
  def toBytes: Array[Byte] =
    HttpResponse.toBytes("404 Not Found", Some("404 - Not Found"))
}

case class ServerErrorResponse(throwable: Throwable) extends HttpResponse {
  def toBytes: Array[Byte] =
    HttpResponse.toBytes("500 Internal Server Error", Some(s"500 - Internal Server Error\n$throwable"))
}

case class OKResponse(content: String) extends HttpResponse {
  def toBytes: Array[Byte] =
    HttpResponse.toBytes("200 OK", Some(content))
}
