package la.hali

import java.nio.charset.StandardCharsets

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.NonUnitStatements"))
class HttpRequestSpec extends FlatSpec with Matchers {

  "A GETRequest" should "be parsed from bytes" in {

    val stringBuilder =
      new StringBuilder()
        .append("GET /hello.txt HTTP/1.1\r\n")
        .append("User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n")
        .append("Host: www.example.com\r\n")
        .append("Accept-Language: en, mi\r\n")
        .append("\r\n")

    val requestString = stringBuilder.toString()

    HttpRequest.fromBytes(ArrayBuffer(requestString.getBytes(StandardCharsets.US_ASCII): _*)) match {
      case None => fail()
      case Some((req, rem)) =>
        rem shouldBe empty
        req match {
          case get: GETRequest => get.path shouldBe "/hello.txt"
        }
    }
  }
}
