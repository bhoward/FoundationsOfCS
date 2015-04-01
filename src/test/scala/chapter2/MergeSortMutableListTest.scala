package chapter2

import util.TestSpec

class MergeSortMutableListTest extends TestSpec {
  import MergeSortMutableList.{mergeSort, LIST, NULL}
  
  "Merge Sort on mutable lists" should "sort an empty list" in {
    mergeSort(NULL) should equal (NULL)
  }
  
  it should "sort a short and simple sample list" in {
    val list = new LIST(3, new LIST(1, new LIST(4, new LIST(1, new LIST(5, NULL)))))
    val result = mergeSort(list)
    result.element should equal (1)
    result.next.element should equal (1)
    result.next.next.element should equal (3)
    result.next.next.next.element should equal (4)
    result.next.next.next.next.element should equal (5)
    result.next.next.next.next.next should equal (NULL)
    // note that the contents of list at this point are messed up; try
    // MergeSortMutableList.printList(list)
  }
  
  /* The rest of these depend on easy ways to construct and copy LISTS, and test them for equality;
   * that's why you should use the built-in types, kids
   */
//  it should "sort a sample list" in {
//    val list = List(3, 14, 15, 92, 65, 35, 89, 79, 32, 38, 46, 26, 43, 38, 32, 79, 50, 28, 84, 19, 71, 69, 39, 93, 75, 10)
//    mergeSort(list) should equal (list.sorted)
//  }
//  
//  it should "sort a sorted list" in {
//    val list = List.range(0, 100)
//    mergeSort(list) should equal (list)
//  }
//  
//  it should "sort a reversed list" in {
//    val list = List.range(100, 0, -1)
//    val expected = List.range(1, 101)
//    mergeSort(list) should equal (expected)
//  }
//  
//  it should "sort random lists" in {
//    forAll { list: List[Int] =>
//      mergeSort(list) should equal (list.sorted)
//    }
//  }
}