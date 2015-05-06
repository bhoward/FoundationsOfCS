package chapter11

import util.TestSpec

class CombinatorParserTest extends TestSpec {
  import CombinatorParser._
  
  "Recursive Descent Parser" should "parse correct input" in {
    val input1 = ""
    val expected1 = Some((Empty, '$'))
    BalancedParens(input1) should be(expected1)
    
    val input2 = "()"
    val expected2 = Some((Parens(Empty, Empty), '$'))
    BalancedParens(input2) should be(expected2)
    
    val input3 = "()()"
    val expected3 = Some((Parens(Empty, Parens(Empty, Empty)), '$'))
    BalancedParens(input3) should be(expected3)
    
    val input4 = "(())"
    val expected4 = Some((Parens(Parens(Empty, Empty), Empty), '$'))
    BalancedParens(input4) should be(expected4)
  }
  
  it should "leave unmatched input behind" in {
    val input1 = ")"
    val expected1 = Some((Empty, ')'))
    BalancedParens(input1) should be(expected1)
    
    val input2 = "() "
    val expected2 = Some((Parens(Empty, Empty), ' '))
    BalancedParens(input2) should be(expected2)
    
    val input3 = "() ()"
    val expected3 = Some((Parens(Empty, Empty), ' '))
    BalancedParens(input3) should be(expected3)
    
    val input4 = "())("
    val expected4 = Some((Parens(Empty, Empty), ')'))
    BalancedParens(input4) should be(expected4)

    val input5 = "("
    val expected5 = Some((Empty, '('))
    BalancedParens(input5) should be(expected5)
    
    val input6 = "()("
    val expected6 = Some((Parens(Empty, Empty), '('))
    BalancedParens(input6) should be(expected6)
    
    val input7 = "(()"
    val expected7 = Some((Empty, '('))
    BalancedParens(input7) should be(expected7)
  }
}