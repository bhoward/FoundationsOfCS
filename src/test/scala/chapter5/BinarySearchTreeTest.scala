package chapter5

import util.TestSpec
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class BinarySearchTreeTest extends TestSpec {
  import BinarySearchTree._
  
  implicit def arbBST: Arbitrary[BST] = Arbitrary {
    for (xs <- arbitrary[List[Int]]) yield insertAll(Empty, xs)
  }
  
  "BST insert" should "contain inserted elements" in {
    forAll { (t: BST, x: Int) =>
      contains(insert(t, x), x) should be (true)
    }
  }

  "BST insertAll" should "contain inserted elements" in {
    forAll { (n: Int, xs: List[Int]) =>
      val t = insertAll(Empty, xs)
      xs.forall(x => contains(t, x)) should be (true)
      contains(t, n) should be (xs.contains(n))
    }
  }
  
  "BST deleteMin" should "remove the smallest element" in {
    forAll { t: BST =>
      val m1 = findMin(t)
      val m2 = findMin(deleteMin(t))
      
      if (m1.isEmpty) {
        m2 shouldBe empty
      } else {
        inorder(deleteMin(t)) should equal(inorder(delete(t, m1.get)))
        if (m2.nonEmpty) {
          m1.get should be <= (m2.get)
        }
      }
    }
  }

  "BST inorder" should "be sorted" in {
    forAll { t: BST =>
      val a = inorder(t)
      a should equal(a.sorted)
    }
  }

  "BST treeSort" should "sort a list of numbers" in {
    forAll { xs: List[Int] =>
      treeSort(xs) should equal(xs.sorted)
    }
  }
}