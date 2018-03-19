package la.hali

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

trait HttpResponse {
  def toBytes: Array[Byte]
}

object NotImplementedResponse extends HttpResponse {
  def toBytes: Array[Byte] = {
    val message = "Not implemented"
    val date = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneId.of("UTC")).format(Instant.now())
    val stringBuilder = new StringBuilder()
      .append("HTTP/1.1 501 Not Implemented\r\n")
      .append(s"Date: $date\r\n")
      .append("Server: la.hali.scala-http-server\r\n")
      .append(s"Content-Length: ${message.getBytes(StandardCharsets.US_ASCII).length}\r\n")
      .append("Content-Type: text/plain\r\n")
      .append("\r\n")
      .append(message)
    stringBuilder.toString().getBytes(StandardCharsets.US_ASCII)
  }
}

object BadRequestResponse extends HttpResponse {
  def toBytes: Array[Byte] = {
    val message = "400 - Bad Request"
    val date = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneId.of("UTC")).format(Instant.now())
    val stringBuilder = new StringBuilder()
      .append("HTTP/1.1 404 Bad Request\r\n")
      .append(s"Date: $date\r\n")
      .append("Server: la.hali.scala-http-server\r\n")
      .append(s"Content-Length: ${message.getBytes(StandardCharsets.US_ASCII).length}\r\n")
      .append("Content-Type: text/plain\r\n")
      .append("\r\n")
      .append(message)
    stringBuilder.toString().getBytes(StandardCharsets.US_ASCII)
  }
}

object NotFoundResponse extends HttpResponse {
  def toBytes: Array[Byte] = {
    val message = "404 - Not Found"
    val date = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneId.of("UTC")).format(Instant.now())
    val stringBuilder = new StringBuilder()
      .append("HTTP/1.1 404 Not Found\r\n")
      .append(s"Date: $date\r\n")
      .append("Server: la.hali.scala-http-server\r\n")
      .append(s"Content-Length: ${message.getBytes(StandardCharsets.US_ASCII).length}\r\n")
      .append("Content-Type: text/plain\r\n")
      .append("\r\n")
      .append(message)
    stringBuilder.toString().getBytes(StandardCharsets.US_ASCII)
  }
}

case class ServerErrorResponse(throwable: Throwable) extends HttpResponse {
  def toBytes: Array[Byte] = {
    val message = s"500 - Internal Server Error\n$throwable"
    val date = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneId.of("UTC")).format(Instant.now())
    val stringBuilder = new StringBuilder()
      .append("HTTP/1.1 500 Internal Server Error\r\n")
      .append(s"Date: $date\r\n")
      .append("Server: la.hali.scala-http-server\r\n")
      .append(s"Content-Length: ${message.getBytes(StandardCharsets.US_ASCII).length}\r\n")
      .append("Content-Type: text/plain\r\n")
      .append("\r\n")
      .append(message)
    stringBuilder.toString().getBytes(StandardCharsets.US_ASCII)
  }
}

case class OKResponse(content: String) extends HttpResponse {
  def toBytes: Array[Byte] = {
    val date = DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneId.of("UTC")).format(Instant.now())
    val stringBuilder = new StringBuilder()
      .append("HTTP/1.1 200 OK\r\n")
      .append(s"Date: $date\r\n")
      .append("Server: la.hali.scala-http-server\r\n")
      .append(s"Content-Length: ${content.getBytes(StandardCharsets.US_ASCII).length}\r\n")
      .append("Content-Type: text/plain\r\n")
      .append("\r\n")
      .append(content)
    stringBuilder.toString().getBytes(StandardCharsets.US_ASCII)
  }
}
