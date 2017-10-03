import org.scalatest.{FeatureSpec, FunSuite, GivenWhenThen}

class StringToolTest extends FunSuite with GivenWhenThen with HuffmanTree with Stopwatch with StringTool {

  val characters = "This planet has - or rather had - a problem, which was this: most\nof  the  people  on  it were unhappy for pretty much of the time.\nMany solutions were suggested for this problem, but most of these\nwere  largely  concerned with the movements of small green pieces\nof paper, which is odd because on the whole it wasn't  the  small\ngreen pieces of paper that were unhappy.\n\nAnd so the problem remained; lots of the people  were  mean,  and\nmost of them were miserable, even the ones with digital watches."
  test("Correct frequence of characters can be calculated") {
    val fc = getCharacterFrequency(characters)
    assert(fc(' ').intValue() == 94)
    assert(fc('e').intValue() == 63)
    assert(fc(',').intValue() == 5)
  }
}
