

object Main extends App {
  var filename_tiny = "EncodeThis_10025"
  var filename_small = "EncodeThis_1098588"
  var filename_medium = "EncodeThis_1529973"
  var filename_large = "EncodeThis_2847448"
  var filename_hardcore = "toCompress"

  Huffman.encode(s"$filename_hardcore.txt")
  Huffman.decode("output")
 }
