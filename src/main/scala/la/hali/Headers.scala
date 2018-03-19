package la.hali

object Headers {
  def apply() = new Headers(Map())
}

class Headers private(backingStore: Map[String, String]) extends Iterable[(String, String)] {
  override def iterator: Iterator[(String, String)] = backingStore.iterator

  def +(kv: (String, String)): Headers = new Headers(backingStore + kv.copy(_1 = kv._1.toLowerCase))

  def get(key: String): Option[String] = backingStore.get(key.toLowerCase)

  def -(key: String): Headers = new Headers(backingStore - key.toLowerCase)

  def apply(key: String): String = backingStore(key.toLowerCase)

  def keySet: Set[String] = backingStore.keySet
}
