import java.io.File

import scala.io.Source

object Huffman extends Stopwatch with FileHandler with ReplacementCodec with HuffmanTree with StringTool {

  def encode(fromFilename: String = "input.txt",
             toFilename: String = "output",
             path: String = System.getProperty("user.home") + File.separator + "Huff" + File.separator): Unit = {

    val text = Source.fromFile(path + fromFilename).mkString
    val nodes = getSortedNodes(getCharacterFrequency(text))
    val encodingMap = getEncodingMapFromTree(getTree(nodes.clone))
    val encodedData = encodeDataWithMap(text, encodingMap)

    saveDataToFile(path + toFilename + ".bin", encodedData)
    saveSortedNodesToFile(path + toFilename + ".tree.bin", getSortedNodes(getCharacterFrequency(text)))
  }

  def decode(fromFilename: String = "input",
             toFilename: String = "output",
             path: String = System.getProperty("user.home") + File.separator + "Huff" + File.separator): Unit = {

    val nodes = loadSortedNodesFromFile(path + fromFilename + ".tree.bin")
    val data = loadDataFromFile(path + fromFilename + ".bin")

    val map = getDecodingMapFromTree(getTree(nodes))

    val decodedData = decodeDataWithMap(data, map)

    saveDataToFile(path + toFilename + ".result.txt", decodedData)
  }
  }
