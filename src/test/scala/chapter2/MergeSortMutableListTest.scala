package chapter2

import util.TestSpec

class MergeSortMutableListTest extends TestSpec {
  import MergeSortMutableList._

  "Merge Sort on mutable lists" should "sort an empty list" in {
    mergeSort(NULL) should equal(NULL)
  }

  it should "sort a short and simple sample list" in {
    val list = makeList(List(3, 1, 4, 1, 5))
    val expected = makeList(List(1, 1, 3, 4, 5))
    val result = mergeSort(list)
    equalLists(expected, result) should be(true)
    // note that the contents of list at this point are messed up; try
    // printList(list)
  }

  it should "sort a sample list" in {
    val original = List(3, 14, 15, 92, 65, 35, 89, 79, 32, 38, 46, 26, 43, 38, 32, 79, 50, 28, 84, 19, 71, 69, 39, 93, 75, 10)
    val list = makeList(original)
    val expected = makeList(original.sorted)
    val result = mergeSort(list)
    equalLists(expected, result) should be(true)
  }
  
  it should "sort a sorted list" in {
    val original = List.range(0, 100)
    val list = makeList(original)
    val expected = makeList(original)
    val result = mergeSort(list)
    equalLists(expected, result) should be(true)
  }

  it should "sort a reversed list" in {
    val original = List.range(100, 0, -1)
    val list = makeList(original)
    val expected = makeList(List.range(1, 101))
    val result = mergeSort(list)
    equalLists(expected, result) should be(true)
  }

  it should "sort random lists" in {
    forAll { original: List[Int] =>
      val list = makeList(original)
      val expected = makeList(original.sorted)
      val result = mergeSort(list)
      equalLists(expected, result) should be(true)
    }
  }
}