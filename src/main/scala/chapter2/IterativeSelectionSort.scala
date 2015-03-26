package chapter2

import scala.collection.mutable.ArrayBuilder

// Based on Figure 2.3 of Aho & Ullman
object IterativeSelectionSort {
  def main(args: Array[String]): Unit = {
    val in = new java.util.Scanner(System.in)
    val AB = new ArrayBuilder.ofInt

    // read and store input in AB
    while (in.hasNextInt) {
      AB += in.nextInt()
    }

    val A = AB.result

    // sort A
    selectionSort(A)

    // print A
    for (i <- 0 until A.length) {
      println(A(i))
    }
  }

  def selectionSort(A: Array[Int]): Unit = {
    for (i <- 0 until A.length - 1) {
      var small = i
      for (j <- i + 1 until A.length) {
        if (A(j) < A(small)) {
          small = j
        }
      }
      val temp = A(small)
      A(small) = A(i)
      A(i) = temp
    }
  }
}
