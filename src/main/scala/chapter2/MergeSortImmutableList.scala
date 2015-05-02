package chapter2

import scala.annotation.tailrec

// Based on Figure 2.31 of Aho & Ullman, using built-in
// immutable lists (but not pattern matching on lists)
object MergeSortImmutableList {
  import java.util.Scanner

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val list = makeList(in)
    printList(mergeSort(list))
  }

  def makeList(in: Scanner): List[Int] = {
    if (!in.hasNextInt) {
      Nil
    } else {
      val x = in.nextInt()
      x :: makeList(in)
    }
  }

  def printList(list: List[Int]): Unit = {
    for (current <- list) {
      println(current)
    }
  }

  def mergeSort(list: List[Int]): List[Int] = {
    if (list == Nil) {
      list
    } else if (list.tail == Nil) {
      list
    } else {
      val (first, second) = list.splitAt(list.size / 2)
      merge(mergeSort(first), mergeSort(second))
    }
  }

  // tail-recursive to avoid stack overflow on large lists
  @tailrec
  def merge(list1: List[Int], list2: List[Int],
            result: List[Int] = Nil): List[Int] = {
    if (list1 == Nil) {
      result.reverse ::: list2
    } else if (list2 == Nil) {
      result.reverse ::: list1
    } else if (list1.head <= list2.head) {
      merge(list1.tail, list2, list1.head :: result)
    } else {
      merge(list1, list2.tail, list2.head :: result)
    }
  }
}
