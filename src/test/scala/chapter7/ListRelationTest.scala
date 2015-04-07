package chapter7

import util.TestSpec

class ListRelationTest extends TestSpec {
  import ListRelation._

  "Linked List lookup" should "find all and only inserted elements" in {
    forAll { ps: List[(Int, Int)] =>
      var L: REL[Int, Int] = Nil
      for ((x, y) <- ps) L = insert(x, y, L)
      for ((x, y) <- ps) {
        lookup(x, L) contains y should be(true)
      }
      forAll { n: Int =>
        whenever(!ps.exists { case (x, y) => x == n }) {
          lookup(n, L) should be(Nil)
        }
      }
    }
  }

  it should "no longer find deleted elements" in {
    forAll { ps: List[(Int, Int)] =>
      var L: REL[Int, Int] = Nil
      for ((x, y) <- ps) L = insert(x, y, L)
      var qs = scala.util.Random.shuffle(ps.distinct)
      while (qs.nonEmpty) {
        val (x, y) = qs.head
        L = delete(x, y, L)
        lookup(x, L) contains y should be(false)
        for ((u, v) <- qs.tail) lookup(u, L) contains v should be(true)
        qs = qs.tail
      }
      L should be(Nil)
      forAll { n: Int =>
        lookup(n, L) should be(Nil)
      }
    }
  }
}
