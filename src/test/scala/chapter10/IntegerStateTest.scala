package chapter10

import util.TestSpec

class IntegerStateTest extends TestSpec {
  import IntegerState._

  "Integer state machine testWord" should "work on examples" in {
    testWord("abstemious") should be(true)
    testWord("sacrilegious") should be(true)
    testWord("undercoating") should be(false) // vowels not in order
    testWord("religious") should be(false) // no letter a
    testWord("aeiou") should be(true)
  }
}
