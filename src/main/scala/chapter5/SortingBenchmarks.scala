package chapter5

import org.scalameter.{config, Key, Warmer, Measurer}
import scala.util.Random

import chapter2.{IterativeSelectionSort, RecursiveSelectionSort, MergeSortImmutableList}

object SortingBenchmarks {
  def benchmark[C](sort: C => Unit, inputs: Seq[C]): Seq[Double] = {
    for (c <- inputs) yield {
      config(Key.exec.benchRuns -> 20)
        .withWarmer(new Warmer.Default)
        .withMeasurer(new Measurer.IgnoringGC)
        .measure {
          sort(c)
        }
    }
  }

  val sizes = 500 to 10000 by 500
  val rand = new Random(0)

  val sortedInputLists = for (size <- sizes) yield List.range(0, size)
  val reverseInputLists = for (size <- sizes) yield List.range(size, 0, -1)
  val randomInputLists = for (size <- sizes) yield List.fill(size)(rand.nextInt)

  val sortedInputArrays = for (list <- sortedInputLists) yield list.toArray
  val reverseInputArrays = for (list <- reverseInputLists) yield list.toArray
  val randomInputArrays = for (list <- randomInputLists) yield list.toArray

  def testArray(name: String, sort: Array[Int] => Unit): Unit = {
    val sortedTimes = benchmark(sort, sortedInputArrays)
    val reverseTimes = benchmark(sort, reverseInputArrays)
    val randomTimes = benchmark(sort, randomInputArrays)

    println(s"$name, sorted inputs:")
    for ((n, t) <- sizes zip sortedTimes) {
      println(s"$n: $t")
    }
    println

    println(s"$name, reverse inputs:")
    for ((n, t) <- sizes zip reverseTimes) {
      println(s"$n: $t")
    }
    println

    println(s"$name, random inputs:")
    for ((n, t) <- sizes zip randomTimes) {
      println(s"$n: $t")
    }
    println
  }

  def testList(name: String, sort: List[Int] => Unit): Unit = {
    val sortedTimes = benchmark(sort, sortedInputLists)
    val reverseTimes = benchmark(sort, reverseInputLists)
    val randomTimes = benchmark(sort, randomInputLists)

    println(s"$name, sorted inputs:")
    for ((n, t) <- sizes zip sortedTimes) {
      println(s"$n: $t")
    }
    println

    println(s"$name, reverse inputs:")
    for ((n, t) <- sizes zip reverseTimes) {
      println(s"$n: $t")
    }
    println

    println(s"$name, random inputs:")
    for ((n, t) <- sizes zip randomTimes) {
      println(s"$n: $t")
    }
    println
  }

  def main(args: Array[String]): Unit = {
    testArray("Iterative Selection Sort", IterativeSelectionSort.selectionSort)
    testArray("Recursive Selection Sort", (A: Array[Int]) => RecursiveSelectionSort.recSelectionSort(A))
    testList("Immutable Merge Sort", MergeSortImmutableList.mergeSort)
//    testList("BST Tree Sort, Naive", BinarySearchTree.treeSortNaive)
//    testList("BST Tree Sort, Tail Recursive", BinarySearchTree.treeSortTail) // too much GC
//    testList("BST Tree Sort, Continuation-Passing", BinarySearchTree.treeSortCPS)
//    testList("BST Tree Sort, Accumulator", BinarySearchTree.treeSortAcc)
//    testList("BST Tree Sort, CPS Accumulator", BinarySearchTree.treeSortCPSAcc)
    testList("AVL Tree Sort", AVLBinarySearchTree.treeSort)
    testList("Binary Heap Sort", BinaryHeap.heapSort)
    testList("Skew Heap Sort", SkewHeap.heapSort)
  }
}
