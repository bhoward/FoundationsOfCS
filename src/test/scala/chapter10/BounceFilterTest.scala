package chapter10

import util.TestSpec

class BounceFilterTest extends TestSpec {
  import BounceFilter._
  
  "Bounce Filter" should "remove spurious bits" in {
    val input = "0101101"
    val expected = "0000111"
    
    bounce(input) should be(expected)
  }
  
  // TODO needs more tests
}
