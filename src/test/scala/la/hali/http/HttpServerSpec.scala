package la.hali.http

import org.scalatest.{FlatSpec, Matchers}

@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.NonUnitStatements"))
class HttpServerSpec extends FlatSpec with Matchers {

  "HttpServer" should "respond to a HEAD request" in {
    val response = HttpServer.respond({
      case g: Get => OK.response("Hello!")
    }, Head("/foo", Headers()))

    response match {
      case ok: HttpResponse with OK =>
        ok.body.length shouldBe 0 // Responses to HEAD requests must not have a body
        ok.headers shouldNot be ('empty)
        ok.statusLine shouldNot be ('empty)
      case _ => fail()
    }
  }

  it should "respond to a GET request" in {
    val response = HttpServer.respond({
      case g: Get => OK.response("Hello!")
    }, Get("/foo", Headers()))

    response match {
      case ok: HttpResponse with OK =>
        new String(ok.body) shouldBe "Hello!"
        ok.headers shouldNot be ('empty)
        ok.statusLine shouldNot be ('empty)
      case _ => fail()
    }
  }

}
