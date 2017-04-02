package scalatorrent.bencode

import org.scalatest._

class bencodeDecodeTest extends FunSuite {

  /**
    * Decode integer tests
    */

  test("Simple number"){
    assert(BencodeDecoder.decodeInt("10000e".getBytes.toList)._1 == 10000)
  }

  test("Negative number"){
    assert(BencodeDecoder.decodeInt("-9e".getBytes.toList)._1 == -9)
  }

  test("Number in string length"){
    assert(BencodeDecoder.decodeInt("-91234567:".getBytes.toList, end = ':')._1 == -91234567)
  }

  test("Zero in string length"){
    assert(BencodeDecoder.decodeInt("0:".getBytes.toList, end = ':')._1 == 0)
  }

  test("Zero"){
    assert(BencodeDecoder.decodeInt("0e".getBytes.toList)._1 == 0)
  }

  test("Empty list") {
    intercept[IllegalArgumentException] {
      BencodeDecoder.decodeInt(Nil)
    }
  }

  test("Wrong char in input") {
    intercept[IllegalArgumentException] {
      BencodeDecoder.decodeInt("1ae".getBytes.toList)
    }
  }

  test("EOF before termination char") {
    intercept[IllegalArgumentException] {
      BencodeDecoder.decodeInt("11233312".getBytes.toList)
    }
  }
}
