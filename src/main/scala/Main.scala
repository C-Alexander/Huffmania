

object Main extends App {
  val filename_tiny = "EncodeThis_10025"
  val filename_small = "EncodeThis_1098588"
  val filename_medium = "EncodeThis_1529973"
  val filename_large = "EncodeThis_2847448"
  val filename_hardcore = "toCompress"

  Huffman.encode(s"$filename_hardcore.txt")
  Huffman.decode("output")
 }
