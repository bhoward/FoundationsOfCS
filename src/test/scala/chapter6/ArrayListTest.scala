package chapter6

import util.TestSpec

class ArrayListTest extends TestSpec {
  import ArrayList._
  
  "ArrayList linear search" should "find all and only inserted elements" in {
    forAll { xs: List[Int] =>
      val L = new LIST
      for (x <- xs) L.insert(x)
      for (x <- xs) {
        L.lookup(x) should be(true)
      }
      forAll { n: Int =>
        L.lookup(n) should be(xs contains n)
      }
    }
  }

  "ArrayList linear search with sentinel" should "find all and only inserted elements" in {
    forAll { xs: List[Int] =>
      val L = new LIST
      for (x <- xs) L.insert(x)
      for (x <- xs) {
        L.lookup2(x) should be(true)
      }
      forAll { n: Int =>
        L.lookup2(n) should be(xs contains n)
      }
    }
  }

  "ArrayList binary search" should "find all and only inserted elements" in {
    forAll { xs: List[Int] =>
      val L = new LIST
      for (x <- xs) L.insertSorted(x)
      for (x <- xs) {
        L.binsearch(x) should be(true)
      }
      forAll { n: Int =>
        L.binsearch(n) should be(xs contains n)
      }
    }
  }
}