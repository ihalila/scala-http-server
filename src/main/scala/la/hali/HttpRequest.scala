package la.hali

import scala.collection.mutable.ArrayBuffer

sealed trait Method
case object GET extends Method
case object HEAD extends Method
case object POST extends Method
case object PUT extends Method
case object DELETE extends Method
case object CONNECT extends Method
case object OPTIONS extends Method
case object TRACE extends Method

sealed trait HttpRequest {
  def path: String
  def method: Method
}

case class GETRequest(override val path: String) extends HttpRequest {
  override val method: GET.type = GET
}

case class MalformedRequest(problem: String)

object HttpRequest {
  /** Attempt to construct an HttpRequest from the given bytes. If successful returns the request
    * and any remaining unconsumed bytes
    */
  def fromBytes(bytes: ArrayBuffer[Byte]): Option[(HttpRequest, ArrayBuffer[Byte])] = {
    val chars = bytes.map(_.toChar)
    if (chars.indexOfSlice("\r\n\r\n") >= 0) {
      val (line, tail) = chars.span(_ != '\r')
      val statusLine = new String(line.toArray)

      val methodSeparator = statusLine.indexOf(' ')
      val method = statusLine.slice(0, methodSeparator)

      val targetSeparator = statusLine.indexOf(' ', methodSeparator + 1)
      val target = statusLine.slice(methodSeparator + 1, targetSeparator)

      val httpVersion = statusLine.slice(targetSeparator + 1, statusLine.length)

      method match {
        case "GET" => Some(GETRequest(target), ArrayBuffer())
      }

    } else {
      None
    }
  }
}