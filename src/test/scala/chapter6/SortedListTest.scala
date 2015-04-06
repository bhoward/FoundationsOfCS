package chapter6

import util.TestSpec

class SortedListTest extends TestSpec {
  import SortedList._

  "Sorted List lookup" should "find all and only inserted elements" in {
    forAll { xs: List[Int] =>
      var L: LIST = Nil
      for (x <- xs) L = insert(x, L)
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
      var L: LIST = Nil
      for (x <- xs) L = insert(x, L)
      var ys = scala.util.Random.shuffle(xs.distinct)
      while (ys.nonEmpty) {
        L = delete(ys.head, L)
        lookup(ys.head, L) should be(false)
        for (y <- ys.tail) lookup(y, L) should be(true)
        ys = ys.tail
      }
      L should be(Nil)
      forAll { n: Int =>
        lookup(n, L) should be(false)
      }
    }
  }
}
