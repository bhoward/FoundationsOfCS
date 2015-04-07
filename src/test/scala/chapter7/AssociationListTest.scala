package chapter7

import util.TestSpec

class AssociationListTest extends TestSpec {
  import AssociationList._

  "Association List lookup" should "find all and only inserted elements" in {
    forAll { ps: List[(String, Int)] =>
      var L: AList[String, Int] = Nil
      for ((s, x) <- ps) L = insert(s, x, L)
      for ((s, _) <- ps) {
        val x = (for ((t, y) <- ps if t == s) yield y).last
        lookup(s, L) should be(Some(x))
      }
      forAll { t: String =>
        whenever(!ps.exists { case (s, x) => s == t }) {
          lookup(t, L) should be(None)
        }
      }
    }
  }

  it should "no longer find deleted elements" in {
    forAll { ps: List[(String, Int)] =>
      var L: AList[String, Int] = Nil
      for ((s, x) <- ps) L = insert(s, x, L)
      val keys = (for ((s, _) <- ps) yield s).distinct
      var qs = scala.util.Random.shuffle(keys)
      while (qs.nonEmpty) {
        L = delete(qs.head, L)
        lookup(qs.head, L) should be(None)
        for (t <- qs.tail) lookup(t, L).nonEmpty should be(true)
        qs = qs.tail
      }
      L should be(Nil)
      forAll { s: String =>
        lookup(s, L) should be(None)
      }
    }
  }
}
