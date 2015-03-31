package chapter5

import util.TestSpec
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class SkewHeapTest extends TestSpec {
  import SkewHeap._
  
  implicit def arbHeap: Arbitrary[Heap] = Arbitrary {
    for (xs <- arbitrary[List[Int]]) yield { insertAll(Empty, xs) }
  }
  
  "Skew Heap insert" should "contain inserted elements" in {
    forAll { (h: Heap, x: Int) =>
      removeAll(insert(h, x)).contains(x) should be (true)
    }
  }

  "Skew Heap insertAll" should "contain inserted elements" in {
    forAll { xs: List[Int] =>
      val a = removeAll(insertAll(Empty, xs))
      a.size should equal(xs.size)
      xs.forall(x => a.contains(x)) should be (true)
      a.forall(x => xs.contains(x)) should be (true)
    }
  }

  "Skew Heap deleteMin" should "remove the smallest element" in {
    forAll { h: Heap =>
      val m1 = findMin(h)
      val m2 = findMin(deleteMin(h))
      
      if (m1.isEmpty) {
        m2 shouldBe empty
      } else {
        if (m2.nonEmpty) {
          m1.get should be <= (m2.get)
        }
      }
    }
  }

  "Skew Heap removeAll" should "be sorted" in {
    forAll { h: Heap =>
      val a = removeAll(h)
      a should equal(a.sorted)
    }
  }

  "Skew Heap Sort" should "sort an empty list" in {
    heapSort(Nil) should equal (Nil)
  }
  
  it should "sort a sample list" in {
    val list = List(3, 14, 15, 92, 65, 35, 89, 79, 32, 38, 46, 26, 43, 38, 32, 79, 50, 28, 84, 19, 71, 69, 39, 93, 75, 10)
    heapSort(list) should equal (list.sorted)
  }
  
  it should "sort a sorted list" in {
    val list = List.range(0, 100)
    heapSort(list) should equal (list)
  }
  
  it should "sort a reversed list" in {
    val list = List.range(100, 0, -1)
    val expected = List.range(1, 101)
    heapSort(list) should equal (expected)
  }
  
  it should "sort random lists" in {
    forAll { list: List[Int] =>
      heapSort(list) should equal (list.sorted)
    }
  }

  it should "sort a large sorted list without overflow" in {
    val xs = List.range(1, 100000)
    heapSort(xs) should equal(xs)
  }
}