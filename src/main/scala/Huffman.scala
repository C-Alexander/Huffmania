import java.io.File

import scala.io.Source

/**
  * A huffman file encoder/decoder
  */
object Huffman extends Stopwatch with FileHandler with ReplacementCodec with HuffmanTree with StringTool {

  /**
    * Encode a file to a (new) file
    * @param fromFilename including extension to encode
    * @param toFilename <b>without extension</b> to save to. The data will be appended .bin, the tree .tree.bin
    * @param path to get files from and save files to
    */
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

  /**
    * Decode a file to a (new) file
    * @param fromFilename <b>without extension</b> to load from.
    * @param toFilename <b>without extension</b> to save the decoded file to
    * @param path to get files from and save files to
    */
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
