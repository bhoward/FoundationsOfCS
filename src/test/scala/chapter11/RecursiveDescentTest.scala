package chapter11

import util.TestSpec

class RecursiveDescentTest extends TestSpec {
  import RecursiveDescent._
  
  def BT(first: TREE, second: TREE): TREE = TREE('B', List(TREE('(', Nil), first, TREE(')', Nil), second))
  val ET: TREE = TREE('e', Nil)
  
  "Recursive Descent Parser" should "parse correct input" in {
    val input1 = ""
    val expected1 = Some((ET, ""))
    B(input1) should be(expected1)
    
    val input2 = "()"
    val expected2 = Some((BT(ET, ET), ""))
    B(input2) should be(expected2)
    
    val input3 = "()()"
    val expected3 = Some((BT(ET, BT(ET, ET)), ""))
    B(input3) should be(expected3)
    
    val input4 = "(())"
    val expected4 = Some((BT(BT(ET, ET), ET), ""))
    B(input4) should be(expected4)
  }
  
  it should "leave unmatched input behind" in {
    val input1 = ")"
    val expected1 = Some((ET, ")"))
    B(input1) should be(expected1)
    
    val input2 = "() "
    val expected2 = Some((BT(ET, ET), " "))
    B(input2) should be(expected2)
    
    val input3 = "() ()"
    val expected3 = Some((BT(ET, ET), " ()"))
    B(input3) should be(expected3)
    
    val input4 = "())("
    val expected4 = Some((BT(ET, ET), ")("))
    B(input4) should be(expected4)
  }
  
  it should "fail on malformed input" in {
    val input1 = "("
    val expected1 = None
    B(input1) should be(expected1)
    
    val input2 = "()("
    val expected2 = None
    B(input2) should be(expected2)
    
    val input3 = "(()"
    val expected3 = None
    B(input3) should be(expected3)
  }
}