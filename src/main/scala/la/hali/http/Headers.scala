package la.hali.http

object Headers {
  def apply(): Headers = new Headers(Map())
  def apply(headers: Map[String, String]): Headers = new Headers(headers)
}

class Headers private(private val backingStore: Map[String, String]) extends Iterable[(String, String)] {
  override def iterator: Iterator[(String, String)] = backingStore.iterator

  def +(kv: (String, String)): Headers = new Headers(backingStore + kv.copy(_1 = kv._1.toLowerCase))

  def get(key: String): Option[String] = backingStore.get(key.toLowerCase)

  def -(key: String): Headers = new Headers(backingStore - key.toLowerCase)

  def apply(key: String): String = backingStore(key.toLowerCase)

  def keySet: Set[String] = backingStore.keySet

  def ++(other: Headers): Headers = new Headers(backingStore ++ other.backingStore)

  override def toString(): String =
    backingStore.foldLeft(new StringBuilder())((sb, h) => sb.append(s"${h._1}: ${h._2}\r\n")).toString()
}
