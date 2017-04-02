package scalatorrent.bencode

import akka.util.ByteString

object BencodeEncoder {

  def apply(message: Any): Array[Byte] = encodeExpr(message)

  /** Bencode delimiters */
  val i = 'i'.toByte
  val l = 'l'.toByte
  val d = 'd'.toByte
  val e = 'e'.toByte

  def encodeExpr(message: Any): Array[Byte] = message match {
    case m: Int => encodeInt(m)
    case m: String => encodeString(ByteString.fromString(m))
    case m: ByteString => encodeString(m)
    case m: List[_] => encodeList(m)
    case m: Map[_, _] => encodeMap(m.asInstanceOf[Map[String, Any]])
    case _ => throw new IllegalArgumentException("Non-valid input for encoding function found")
  }

  def encodeInt(m: Int): Array[Byte] = {
    i +: m.toString.getBytes :+ e
  }

  def encodeString(m: ByteString): Array[Byte] = {
    m.length.toString.getBytes ++ (':'.toByte +: m.toArray)
  }

  def encodeMap(m: Map[String, Any]): Array[Byte] = {
    val inner = m.toSeq.sortBy(_._1).foldLeft(Array[Byte]()) {
        (acc, el) => acc ++ encodeString(ByteString.fromString(el._1)) ++ encodeExpr(el._2)
      }
    d +: inner :+ e
  }

  def encodeList(m: List[Any]): Array[Byte] = {
    l +: m.foldLeft(Array[Byte]()){
      (acc, el) =>
        acc ++ encodeExpr(el)
    } :+ e
  }
}
