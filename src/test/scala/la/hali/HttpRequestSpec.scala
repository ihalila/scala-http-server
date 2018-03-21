package la.hali

import java.nio.charset.StandardCharsets

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.NonUnitStatements"))
class HttpRequestSpec extends FlatSpec with Matchers {

  "A Get" should "be parsed from bytes" in {

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

  "A Post" should "be parsed from bytes" in {

    val requestString =
      new StringBuilder()
        .append("POST /api/nums HTTP/1.1\r\n")
        .append("User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\n")
        .append("Host: www.hali.la\r\n")
        .append("Accept-Language: fi\r\n")
        .append("Content-Length: 17\r\n")
        .append("\r\n")
        .append("12345678901234567")
        .toString()

    HttpRequest.fromBytes(ArrayBuffer(requestString.getBytes(StandardCharsets.US_ASCII): _*)) match {
      case Done(req, rem) =>
        rem shouldBe empty
        req match {
          case post: Post =>
            post.path shouldBe "/api/nums"
            post.headers.keySet.size shouldBe 4
            post.headers("User-Agent") shouldBe "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3"
            post.headers("Host") shouldBe "www.hali.la"
            post.headers("Accept-Language") shouldBe "fi"
            post.headers("Content-Length") shouldBe "17"
            post.body shouldBe Array(49, 50, 51, 52, 53, 54, 55, 56, 57, 48, 49, 50, 51, 52, 53, 54, 55)
          case _ => fail()
        }
      case _ => fail()
    }
  }
}
