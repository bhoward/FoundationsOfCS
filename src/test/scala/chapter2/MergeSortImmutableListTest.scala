package chapter2

import util.TestSpec

class MergeSortImmutableListTest extends TestSpec {
  import MergeSortImmutableList.mergeSort
  
  "Merge Sort on immutable lists" should "sort an empty list" in {
    mergeSort(Nil) should equal (Nil)
  }
  
  it should "sort a sample list" in {
    val list = List(3, 14, 15, 92, 65, 35, 89, 79, 32, 38, 46, 26, 43, 38, 32, 79, 50, 28, 84, 19, 71, 69, 39, 93, 75, 10)
    mergeSort(list) should equal (list.sorted)
  }
  
  it should "sort a sorted list" in {
    val list = List.range(0, 100)
    mergeSort(list) should equal (list)
  }
  
  it should "sort a reversed list" in {
    val list = List.range(100, 0, -1)
    val expected = List.range(1, 101)
    mergeSort(list) should equal (expected)
  }
  
  it should "sort random lists" in {
    forAll { list: List[Int] =>
      mergeSort(list) should equal (list.sorted)
    }
  }
}