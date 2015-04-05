package chapter6

import util.TestSpec

class StacksTest extends TestSpec {
  import Stacks._

  def runTests(stack: Stack, name: String): Unit = {
    s"$name Stack insertion" should "return elements in reverse order" in {
      forAll { xs: List[Int] =>
        stack.clear()
        for (x <- xs) stack.push(x)
        for (x <- xs.reverse) {
          stack.peek should be(Some(x))
          stack.pop() should be(true)
        }
        stack.isEmpty should be(true)
        stack.peek should be(None)
        stack.pop() should be(false)
      }
    }

    it should "work at least up to a large number of elements" in {
      import org.scalacheck.Gen
      import org.scalacheck.Arbitrary.arbitrary

      val bigLists = Gen.listOfN(200, arbitrary[Int])

      forAll(bigLists) { xs: List[Int] =>
        stack.clear()
        var count = 0
        for (x <- xs if !stack.isFull) {
          stack.push(x)
          count += 1
        }
        stack.isEmpty should be(false)
        for (x <- xs.take(count).reverse) {
          stack.peek should be(Some(x))
          stack.pop() should be(true)
        }
        stack.isEmpty should be(true)
        stack.peek should be(None)
        stack.pop() should be(false)
      }
    }
  }

  runTests(new ArrayStack, "Array-based")
  runTests(new ListStack, "List-based")
  runTests(new MutableListStack, "Mutable List-based")
  runTests(new DoublyLinkedListStack, "Doubly-linked List-based")
}