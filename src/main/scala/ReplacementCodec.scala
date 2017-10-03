import java.util

import Huffman.measureTime

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A trait for coding backwards and forwards using Huffman
  */
trait ReplacementCodec {
  this: Stopwatch =>

  /**
    * Encode data using a hashmap
    * @param data to encode
    * @param map map with the characters to replace, and the bits to replace it with as booleans
    * @return An Array[Byte] of the replaced bits. Very efficient!
    */
  protected def encodeDataWithMap(data: String, map: mutable.HashMap[Char, ArrayBuffer[Boolean]]): Array[Byte] = {
    measureTime {
      val jb: java.util.BitSet = new util.BitSet()
      var i: Int = 0
      for (c <- data) {
        for (q <- map(c)) {
          jb.set(i, q)
          i += 1
        }
      }
      jb.toByteArray
    }
  }

  /**
    * Decode data using a hashmap
    * @param data to Decode
    * @param map map with the bits as booleans to replace, and the characters to replace them with
    * @return The text resultant from the decoding as a string
    */
  protected def decodeDataWithMap(data: Array[Byte], map: mutable.HashMap[ArrayBuffer[Boolean], Char]): String = {
    measureTime({
      val bitSet: java.util.BitSet = util.BitSet.valueOf(data)
      var buffer = ArrayBuffer[Boolean]()
      val result = new StringBuilder(bitSet.size() * 8)
      for (i <- 0 until bitSet.previousSetBit(bitSet.size()) + 1) {
        buffer += bitSet.get(i)
        if (map.contains(buffer)) {
          result append map(buffer)
          buffer.clear()
        }
      }
      while (buffer.nonEmpty) {
        buffer append false
        if (map.contains(buffer)) {
          result append map(buffer)
          buffer.clear()
        }
      }
      result.toString
    }, "Decoding Data with Map")
  }
}
