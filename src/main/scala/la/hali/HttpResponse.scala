package la.hali

import java.nio.charset.StandardCharsets

object HttpResponse {
  def error(explanation: String): Array[Byte] = {
    val stringBuilder = new StringBuilder()
      .append("HTTP/1.1 200 OK\r\n")
      .append("Date: Mon, 27 Jul 2009 12:28:53 GMT\r\n")
      .append("Server: la.hali.scala-http-server\r\n")
      .append("Last-Modified: Sun, 3 Mar 2018 10:41:32 EET\r\n")
      .append("ETag: \"34aa387-d-1568eb00\"\r\n")
      .append("Accept-Ranges: bytes\r\n")
      .append("Content-Length: 51\r\n")
      .append("Vary: Accept-Encoding\r\n")
      .append("Content-Type: text/plain\r\n")
      .append("\r\n")
      .append("Hello World! My payload includes a trailing CRLF.\r\n")
    stringBuilder.toString().getBytes(StandardCharsets.US_ASCII)
  }
}

object ErrorResponse extends HttpResponse {
  def toBytes: Array[Byte] = {
    val stringBuilder = new StringBuilder()
      .append("HTTP/1.1 200 OK\r\n")
      .append("Date: Mon, 27 Jul 2009 12:28:53 GMT\r\n")
      .append("Server: la.hali.scala-http-server\r\n")
      .append("Last-Modified: Sun, 3 Mar 2018 10:41:32 EET\r\n")
      .append("ETag: \"34aa387-d-1568eb00\"\r\n")
      .append("Accept-Ranges: bytes\r\n")
      .append("Content-Length: 51\r\n")
      .append("Vary: Accept-Encoding\r\n")
      .append("Content-Type: text/plain\r\n")
      .append("\r\n")
      .append("Hello World! My payload includes a trailing CRLF.\r\n")
    stringBuilder.toString().getBytes(StandardCharsets.US_ASCII)
  }
}

trait HttpResponse {
  def toBytes: Array[Byte]
}