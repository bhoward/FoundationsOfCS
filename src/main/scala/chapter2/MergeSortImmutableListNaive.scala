package chapter2

import scala.annotation.tailrec

// Based on Figure 2.31 of Aho & Ullman, using built-in
// immutable lists (but not pattern matching on lists);
// this version will overflow on large lists because merge
// is not tail-recursive
object MergeSortImmutableListNaive {
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

  def merge(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list1 == Nil) {
      list2
    } else if (list2 == Nil) {
      list1
    } else if (list1.head <= list2.head) {
      list1.head :: merge(list1.tail, list2)
    } else {
      list2.head :: merge(list1, list2.tail)
    }
  }
}
