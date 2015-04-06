package chapter6

import util.TestSpec

class DoublyLinkedListTest extends TestSpec {
  import DoublyLinkedList._

  "Doubly-Linked List lookup" should "find all and only back-inserted elements" in {
    forAll { xs: List[Int] =>
      var L = DoublyLinkedList.empty
      for (x <- xs) insertBack(x, L)
      for (x <- xs) {
        lookup(x, L) should be(true)
      }
      forAll { n: Int =>
        lookup(n, L) should be(xs contains n)
      }
    }
  }

  it should "find all and only front-inserted elements" in {
    forAll { xs: List[Int] =>
      var L = DoublyLinkedList.empty
      for (x <- xs) insertFront(x, L)
      for (x <- xs) {
        lookup(x, L) should be(true)
      }
      forAll { n: Int =>
        lookup(n, L) should be(xs contains n)
      }
    }
  }

  it should "no longer find deleted elements" in {
    forAll { xs: List[Int] =>
      var L = DoublyLinkedList.empty
      for (x <- xs) insertBack(x, L)
      var ys = scala.util.Random.shuffle(xs)
      while (ys.nonEmpty) {
        delete(ys.head, L)
        lookup(ys.head, L) should be(ys.tail contains ys.head)
        for (y <- ys.tail) lookup(y, L) should be(true)
        ys = ys.tail
      }
      isEmpty(L) should be(true)
      forAll { n: Int =>
        lookup(n, L) should be(false)
      }
    }
  }

  "Doubly-Linked List insertFront/deleteBack" should "return elements in original order" in {
    forAll { xs: List[Int] =>
      var L = DoublyLinkedList.empty
      for (x <- xs) insertFront(x, L)
      for (x <- xs) {
        back(L) should be(Some(x))
        deleteBack(L)
      }
      isEmpty(L) should be(true)
      back(L) should be(None)
      deleteBack(L) // shouldn't throw an exception
    }
  }

  "Doubly-Linked List insertBack/deleteFront" should "return elements in original order" in {
    forAll { xs: List[Int] =>
      var L = DoublyLinkedList.empty
      for (x <- xs) insertBack(x, L)
      for (x <- xs) {
        front(L) should be(Some(x))
        deleteFront(L)
      }
      isEmpty(L) should be(true)
      front(L) should be(None)
      deleteFront(L) // shouldn't throw an exception
    }
  }
}
