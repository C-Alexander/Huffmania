import org.scalatest.FunSuite

class ReplacementCodecTest extends FunSuite with ReplacementCodec with HuffmanTree with Stopwatch with StringTool {
val text = "This planet has - or rather had - a problem, which was this: most\nof  the  people  on  it were unhappy for pretty much of the time.\nMany solutions were suggested for this problem, but most of these\nwere  largely  concerned with the movements of small green pieces\nof paper, which is odd because on the whole it wasn't  the  small\ngreen pieces of paper that were unhappy.\n\nAnd so the problem remained; lots of the people  were  mean,  and\nmost of them were miserable, even the ones with digital watches."
var bytes = Array[Byte]()
  test("Can encode data with map") {
    val map2 = getEncodingMapFromTree(getTree(getSortedNodes(getCharacterFrequency(text))))
    bytes = encodeDataWithMap(text, map2)
  }

  var newText = ""
  test("Can decode data with map") {
    val map = getDecodingMapFromTree(getTree(getSortedNodes(getCharacterFrequency(text))))
    newText = decodeDataWithMap(bytes, map)
  }

  test("Decoded map equals encoded map") {
    assert(newText == text)
  }

}
