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
      case Done(req, rem) =>
        rem shouldBe empty
        req match {
          case get: Get =>
            get.path shouldBe "/hello.txt"
            get.headers.keySet.size shouldBe 3
            get.headers("User-Agent") shouldBe "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3"
            get.headers("Host") shouldBe "www.example.com"
            get.headers("Accept-Language") shouldBe "en, mi"
          case _ => fail()
        }
      case _ => fail()
    }
  }

  it should "parse two consecutive GETs" in {
    val stringBuilder =
      new StringBuilder()
        .append("GET /hello.txt HTTP/1.1\r\n")
        .append("User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n")
        .append("Host: www.example.com\r\n")
        .append("Accept-Language: en, mi\r\n")
        .append("\r\n")
        .append("GET /foo/bar HTTP/1.1\r\n")
        .append("User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n")
        .append("Host: www.example.com\r\n")
        .append("Accept-Language: en, mi\r\n")
        .append("\r\n")

    val requestString = stringBuilder.toString()
    val reqBytes = ArrayBuffer(requestString.getBytes(StandardCharsets.US_ASCII): _*)

    val Done(req1, rem) = HttpRequest.fromBytes(reqBytes)
    val Done(req2, _) = HttpRequest.fromBytes(rem)

    req1.path shouldBe "/hello.txt"
    req2.path shouldBe "/foo/bar"
  }
}
