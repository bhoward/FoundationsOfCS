package chapter2

import util.TestSpec

class IterativeSelectionSortTest extends TestSpec {
  import IterativeSelectionSort.selectionSort
  
  "Iterative Selection Sort" should "sort an empty array" in {
    val A = new Array[Int](0)
    selectionSort(A)
  }
  
  it should "sort a sample array" in {
    val A = Array(3, 14, 15, 92, 65, 35, 89, 79, 32, 38, 46, 26, 43, 38, 32, 79, 50, 28, 84, 19, 71, 69, 39, 93, 75, 10)
    val E = A.clone.sorted
    A should not equal (E)
    selectionSort(A)
    A should equal (E)
  }
  
  it should "sort a sorted array" in {
    val A = Array.range(0, 100)
    val E = A.clone
    selectionSort(A)
    A should equal (E)
  }

  it should "sort a reversed array" in {
    val A = Array.range(100, 0, -1)
    val E = Array.range(1, 101)
    selectionSort(A)
    A should equal (E)
  }
  
  it should "sort random arrays" in {
    forAll { A: Array[Int] =>
      val E = A.clone.sorted
      selectionSort(A)
      A should equal (E)
    }
  }
  
  it should "sort a large sorted array without overflow" in {
    val A = Array.range(1, 100000)
    val E = A.clone
    selectionSort(A)
    A should equal(E)
  }
}
