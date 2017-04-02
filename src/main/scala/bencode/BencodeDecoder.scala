package scalatorrent.bencode

import akka.util.ByteString
import scalatorrent.util.{Const}
object BencodeDecoder {

  def apply(code: List[Byte]): Any = {
    decodeExpr(code)._1
  }

  def isDigit(num: Byte): Boolean = num >= '0' && num <= '9'


  /** Return decoded int. By changing end char can be used to decode string length */
  def decodeInt(code: List[Byte], end: Char = 'e'): (Int, List[Byte]) = {
    def decodeInt0(code: List[Byte], acc: Int = 0): (Int, List[Byte]) = code match {
      case x :: xs if x == end => (acc, xs)
      case x :: xs if isDigit(x) => decodeInt0(xs, acc * 10 + (x - '0'))
      case x :: _ => throw new IllegalArgumentException(s"Found illegal char $x before int termination char 'e'")
      case Nil => throw new IllegalArgumentException("Found EOF before int termination char")
    }

    code match {
      case '-' :: xs =>
        val (decoded, rest) = decodeInt0(xs)
        (-1 * decoded, rest)
      case _ => decodeInt0(code)
    }
  }

  def decodeList(code: List[Byte],
                 acc: List[Any] = Nil): (List[Any], List[Byte]) = code match {
    case Nil => throw new IllegalAccessException("Found EOF before list termination char")
    case 'e' :: xs => (acc, xs)
    case _ =>
      val (decoded, rest) = decodeExpr(code)
      decodeList(rest, decoded :: acc)
  }

  def decodeDictionary(code: List[Byte],
                       acc: Map[String, Any] = Map()): (Map[String, Any], List[Byte]) = code match {
    case Nil => throw new IllegalArgumentException("Found EOF before dictionary termination char")
    case 'e' :: xs => (acc, xs)
    case _ =>
      val (key, rest) = decodeString(code)
      val (data, rest0) = decodeExpr(rest)
      decodeDictionary(rest0, acc + (key.decodeString(Const.Charset) -> data))
  }

  def decodeString(code: List[Byte]): (ByteString, List[Byte]) = {
    val (length, rest) = decodeInt(code, end = ':')

    val (string, tail) = rest.splitAt(length)

    (ByteString.fromArray(string.toArray), tail)
  }

  def decodeExpr(code: List[Byte]): (Any, List[Byte]) = code match {
    case 'i' :: xs => decodeInt(xs)
    case 'd' :: xs => decodeDictionary(xs)
    case 'l' :: xs => decodeList(xs)
    case x :: _ if isDigit(x) => decodeString(code)
    case x :: _ => throw new IllegalArgumentException(s"Non-bencode symbol $x on top level")
    case Nil => (Nil, Nil)
  }


}

