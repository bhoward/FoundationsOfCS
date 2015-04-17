package chapter10

import util.TestSpec

class IntegerStateVendingTest extends TestSpec {
  import IntegerStateVending._

  "Integer state vending machine" should "produce correct output" in {
    val input = "NNNNQDNND"
    val expected =
      """Candy!
        |Candy!
        |Candy!
        |""".stripMargin

    outputFrom(vend(input)) should be(expected)
  }
}
