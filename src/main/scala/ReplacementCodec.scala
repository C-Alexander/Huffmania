import java.util

import Huffman.measureTime

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait ReplacementCodec {
  this: Stopwatch =>

  //5
  def encodeDataWithMap(data: String, map: mutable.HashMap[Char, ArrayBuffer[Boolean]]): Array[Byte] = {
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

  //7
  def decodeDataWithMap(data: Array[Byte], map: mutable.HashMap[ArrayBuffer[Boolean], Char]): String = {
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
