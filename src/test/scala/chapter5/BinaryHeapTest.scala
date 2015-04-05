package chapter5

import util.TestSpec
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class BinaryHeapTest extends TestSpec {
  import BinaryHeap._

  implicit def arbHeap: Arbitrary[Heap] = Arbitrary {
    val h = new Heap
    for (xs <- arbitrary[List[Int]]) yield { h.insertAll(xs); h }
  }

  "Heap insert" should "contain inserted elements" in {
    forAll { (h: Heap, x: Int) =>
      h.insert(x)
      removeAll(h).contains(x) should be(true)
    }
  }

  "Heap insertAll" should "contain inserted elements" in {
    forAll { xs: List[Int] =>
      val h = new Heap
      h.insertAll(xs)
      val a = removeAll(h)
      a.size should equal(xs.size)
      xs.forall(x => a.contains(x)) should be(true)
      a.forall(x => xs.contains(x)) should be(true)
    }
  }

  "Heap deleteMin" should "remove the smallest element" in {
    forAll { h: Heap =>
      val m1 = h.findMin
      h.deleteMin()
      val m2 = h.findMin

      if (m1.isEmpty) {
        m2 shouldBe empty
      } else {
        if (m2.nonEmpty) {
          m1.get should be <= (m2.get)
        }
      }
    }
  }

  "Heap removeAll" should "be sorted" in {
    forAll { h: Heap =>
      val a = removeAll(h)
      a should equal(a.sorted)
    }
  }

  "Heap Sort" should "sort an empty list" in {
    heapSort(Nil) should equal(Nil)
  }

  it should "sort a sample list" in {
    val list = List(3, 14, 15, 92, 65, 35, 89, 79, 32, 38, 46, 26, 43, 38, 32, 79, 50, 28, 84, 19, 71, 69, 39, 93, 75, 10)
    heapSort(list) should equal(list.sorted)
  }

  it should "sort a sorted list" in {
    val list = List.range(0, 100)
    heapSort(list) should equal(list)
  }

  it should "sort a reversed list" in {
    val list = List.range(100, 0, -1)
    val expected = List.range(1, 101)
    heapSort(list) should equal(expected)
  }

  it should "sort random lists" in {
    forAll { list: List[Int] =>
      heapSort(list) should equal(list.sorted)
    }
  }

  it should "sort a large sorted list without overflow" in {
    val xs = List.range(1, 100000)
    heapSort(xs) should equal(xs)
  }
}