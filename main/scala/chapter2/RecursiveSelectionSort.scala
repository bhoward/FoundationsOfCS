package chapter2

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder

// Based on Figure 2.22 of Aho & Ullman
object RecursiveSelectionSort {
  def main(args: Array[String]): Unit = {
    val in = new java.util.Scanner(System.in)
    val AB = new ArrayBuilder.ofInt

    // read and store input in AB
    while (in.hasNextInt) {
      AB += in.nextInt()
    }

    val A = AB.result

    // sort A
    recSelectionSort(A)

    // print A
    for (i <- 0 until A.length) {
      println(A(i))
    }
  }

  def recSelectionSort(A: Array[Int], i: Int = 0): Unit = {
    if (i < A.length - 1) {
      // basis is when i = A.length - 1, in which case
      // the function returns without changing A.
      // induction follows
      var small = i
      for (j <- i + 1 until A.length) {
        if (A(j) < A(small)) {
          small = j
        }
      }
      val temp = A(small)
      A(small) = A(i)
      A(i) = temp
      recSelectionSort(A, i + 1)
    }
  }
}
