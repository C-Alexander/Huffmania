import org.scalatest.FunSuite

class HuffmanTest extends FunSuite with FileHandler with Stopwatch {
  var filename_tiny = "EncodeThis_10025"
  var filename_small = "EncodeThis_1098588"
  var filename_medium = "EncodeThis_1529973"
  var filename_large = "EncodeThis_2847448"
  test("test encoding 10025 words") {
    Huffman.encode(filename_tiny + ".txt")
  }

  test("test decoding 10025 words") {
    Huffman.decode(filename_tiny, filename_tiny)
  }

  test("test encoding 1098588 words") {
    Huffman.encode(filename_small + ".txt", filename_small)
  }

  test("test decoding 1098588 words") {
    Huffman.decode(filename_small, filename_small)
  }

  test("test encoding 1529973 words") {
    Huffman.encode(filename_medium + ".txt", filename_medium)
  }

  test("test decoding 1529973 words") {
    Huffman.decode(filename_medium, filename_medium)
  }

  test("test encoding 2847448 words") {
    Huffman.encode(filename_large + ".txt", filename_large)
  }

  test("test decoding 2847448 words") {
    Huffman.decode(filename_large, filename_large)
  }

}
