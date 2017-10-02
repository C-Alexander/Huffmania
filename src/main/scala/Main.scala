import java.io.File

import Huffman._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, BitSet, ListBuffer}
import scala.io.Source

object Main extends App {
    val dummyText: String = "Zaphod Beeblebrox"
  val ezText: String = "abcd"

    val textToDecode: String = Source.fromFile(System.getProperty("user.home") + File.separator + "Huff" + File.separator + "EncodeThis_2847448.txt").mkString
    if (getCharacterFrequency(textToDecode).size < 2) {
      println("Go huff yourself, shorty!")
      System.exit(0)
    }

//    println(getCharacterFrequency(textToDecode))
//    println(getSortedNodes(getCharacterFrequency(textToDecode)))
//    println(getTree(getSortedNodes(getCharacterFrequency(textToDecode))))
//    println(getEncodingMapFromTree(getTree(getSortedNodes(getCharacterFrequency(textToDecode)))))
//    println(encodeDataWithMap(textToDecode, getEncodingMapFromTree(getTree(getSortedNodes(getCharacterFrequency(textToDecode))))))
  //  println(saveDataToFile(System.getProperty("user.home") + File.separator + "Huff" + File.separator + "DecodeThis_2847448.bin", encodeDataWithMap(textToDecode, getEncodingMapFromTree(getTree(getSortedNodes(getCharacterFrequency(textToDecode)))))))
  var a = getSortedNodes(getCharacterFrequency(textToDecode))
  saveSortedNodesToFile(System.getProperty("user.home") + File.separator + "Huff" + File.separator + "DecodeThis_2847448.tree.bin", a)
  val s = loadSortedNodesFromFile(System.getProperty("user.home") + File.separator + "Huff" + File.separator + "DecodeThis_2847448.tree.bin")
  val gemft = encodeDataWithMap(textToDecode, getEncodingMapFromTree(getTree(a)))
  saveDataToFile(System.getProperty("user.home") + File.separator + "Huff" + File.separator + "DecodeThis_2847448.bin", gemft)
  var t = loadDataFromFile(System.getProperty("user.home") + File.separator + "Huff" + File.separator + "DecodeThis_2847448.bin")
  var p = getDecodingMapFromTree(getTree(s))
  decodeDataWithMap(t, p)

  //  var b = BitSet(0, 2, 4, 6)
//  var bools = ListBuffer[Boolean]() += b.ga


//  var s = new StringBuilder(
//  for(i <- 1 until 500000000) {
//    if (i%2 == 0) b += i
//  }
//  var d = b.range(0, 3)
//  println(b(0).toString + b(1).toString + b(2).toString)
//  println(d(0).toString + d(1).toString + d(2).toString)
//
//  d = b.take(3)
//  println(d(0).toString + d(1).toString + d(2).toString)
//  d = b.take(3)
//  println(d(0).toString + d(1).toString + d(2).toString)
//  d = b.take(2)
//  println(d(0).toString + d(1).toString)
//  d = b.take(1)
//  println(d(0).toString)
//  d = b.slice(0, 4)
//  println(d(0).toString + d(1).toString + d(2).toString)
//  println(b(0).toString + b(1).toString + b(2).toString)
////  (d, b) = b.splitAt(4)
////  println(d(0).toString + d(1).toString + d(2).toString)
////  println(b(0).toString + b(1).toString + b(2).toString)
//
//   val z = b.sliding(3,3).toStream
//  println("stakin")
//  z.foreach(q => println(q(0).toString + q(1).toString + q(2).toString + q(3).toString))
 }
