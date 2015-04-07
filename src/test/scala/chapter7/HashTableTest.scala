package chapter7

import util.TestSpec

class HashTableTest extends TestSpec {
  import HashTable._

  "Hash Table lookup" should "find all and only inserted elements" in {
    forAll { ps: List[(String, Int)] =>
      val T = HashTable.empty[Int]
      for ((s, x) <- ps) insert(s, x, T)
      for ((s, _) <- ps) {
        val x = (for ((t, y) <- ps if t == s) yield y).last
        lookup(s, T) should be(Some(x))
      }
      forAll { t: String =>
        whenever(!ps.exists { case (s, x) => s == t }) {
          lookup(t, T) should be(None)
        }
      }
    }
  }

  it should "no longer find deleted elements" in {
    forAll { ps: List[(String, Int)] =>
      val T = HashTable.empty[Int]
      for ((s, x) <- ps) insert(s, x, T)
      val keys = (for ((s, _) <- ps) yield s).distinct
      var qs = scala.util.Random.shuffle(keys)
      while (qs.nonEmpty) {
        delete(qs.head, T)
        lookup(qs.head, T) should be(None)
        for (t <- qs.tail) lookup(t, T).nonEmpty should be(true)
        qs = qs.tail
      }
      T should be(HashTable.empty[Int])
      forAll { s: String =>
        lookup(s, T) should be(None)
      }
    }
  }
}
