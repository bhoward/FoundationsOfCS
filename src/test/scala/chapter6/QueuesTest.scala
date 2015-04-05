package chapter6

import util.TestSpec

class QueuesTest extends TestSpec {
  import Queues._

  def runTests(queue: Queue, name: String): Unit = {
    s"$name Stack insertion" should "return elements in original order" in {
      forAll { xs: List[Int] =>
        queue.clear()
        for (x <- xs) queue.enqueue(x)
        for (x <- xs) {
          queue.peek should be(Some(x))
          queue.dequeue() should be(true)
        }
        queue.isEmpty should be(true)
        queue.peek should be(None)
        queue.dequeue() should be(false)
      }
    }

    it should "work at least up to a large number of elements" in {
      import org.scalacheck.Gen
      import org.scalacheck.Arbitrary.arbitrary

      val bigLists = Gen.listOfN(200, arbitrary[Int])

      forAll(bigLists) { xs: List[Int] =>
        queue.clear()
        var count = 0
        for (x <- xs if !queue.isFull) {
          queue.enqueue(x)
          count += 1
        }
        queue.isEmpty should be(false)
        for (x <- xs.take(count)) {
          queue.peek should be(Some(x))
          queue.dequeue() should be(true)
        }
        queue.isEmpty should be(true)
        queue.peek should be(None)
        queue.dequeue() should be(false)
      }
    }
  }

  runTests(new ArrayQueue, "Array-based")
  runTests(new ListQueue, "List-based")
  runTests(new MutableListQueue, "Mutable List-based")
  runTests(new DoublyLinkedListQueue, "Doubly-linked List-based")
}