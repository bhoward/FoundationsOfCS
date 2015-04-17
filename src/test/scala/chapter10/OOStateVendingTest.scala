package chapter10

import util.TestSpec

class OOStateVendingTest extends TestSpec {
  import OOStateVending._

  "Object-oriented state vending machine" should "produce correct output" in {
    val input = "NNNNQDNND"
    val expected =
      """Candy!
        |Candy!
        |Candy!
        |""".stripMargin

    outputFrom(vend(input)) should be(expected)
  }
}
