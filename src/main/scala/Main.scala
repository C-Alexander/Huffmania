

object Main extends App {

  val filename = "EncodeThis_2847448"
  Huffman.encode(s"$filename.txt")
  Huffman.decode("output")
 }
