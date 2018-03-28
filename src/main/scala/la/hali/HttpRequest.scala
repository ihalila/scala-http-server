package la.hali

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

final case class Head(override val path: String, override val headers: Headers) extends HttpRequest {
  override def toString: String = s"HEAD: $path"
}

final case class UnknownMethod(override val path: String, override val headers: Headers) extends HttpRequest

